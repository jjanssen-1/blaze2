#include "llvmbackend.h"
#include "frontend/Ir.h"

#include "core/Source.h"

#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

#include <unordered_map>
#include <vector>

// LLVM 16+ moved Host.h into TargetParser/ and replaced llvm::Optional with
// std::optional. We use the version macro to stay compatible with both the
// older (14/15) and newer (16+) API surfaces.
#if LLVM_VERSION_MAJOR >= 16
#include "llvm/TargetParser/Host.h"
#include <optional>
#else
#include "llvm/ADT/Optional.h"
#include "llvm/Support/Host.h"
#endif

blaze::backend::LLVMBackend::LLVMBackend(core::DiagnosticList &diagnostics)
    : m_diagnostics(diagnostics) {
  m_context = std::make_unique<llvm::LLVMContext>();

  m_typeMap[frontend::IRType::I32] = llvm::Type::getInt32Ty(*m_context);
  m_typeMap[frontend::IRType::Bool] = llvm::Type::getInt1Ty(*m_context);
  m_typeMap[frontend::IRType::Void] = llvm::Type::getVoidTy(*m_context);
}

blaze::backend::CodeBackend::Status blaze::backend::LLVMBackend::generate(
    const std::vector<frontend::IRFunction> &functions) {
  m_module = std::make_unique<llvm::Module>("blaze_module", *m_context);
  m_builder = std::make_unique<llvm::IRBuilder<>>(*m_context);

  // First pass: declare all functions so that any call to a peer function
  // resolves to the real declaration rather than a forward stub.
  std::vector<llvm::Function *> llvmFunctions;
  llvmFunctions.reserve(functions.size());
  for (const auto &func : functions) {
    llvmFunctions.push_back(declareFunction(func));
  }

  // Second pass: emit bodies.
  for (size_t i = 0; i < functions.size(); ++i) {
    emitBlocks(functions[i], llvmFunctions[i]);
  }

  if (m_diagnostics.hasErrors())
    return Error;

  return Success;
}

bool blaze::backend::LLVMBackend::emitIRToFile(const std::string &path,
                                               std::string *error) const {
  auto setError = [&](const std::string &message) {
    if (error)
      *error = message;
  };

  if (!m_module) {
    setError("No LLVM module available.");
    return false;
  }

  std::error_code ec;
  llvm::raw_fd_ostream out(path, ec, llvm::sys::fs::OF_Text);
  if (ec) {
    setError(ec.message());
    return false;
  }

  m_module->print(out, nullptr);
  return true;
}

bool blaze::backend::LLVMBackend::emitObjectFile(const std::string &path,
                                                 std::string *error) {
  auto setError = [&](const std::string &message) {
    if (error)
      *error = message;
  };

  if (!m_module) {
    setError("No LLVM module available.");
    return false;
  }

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  const std::string targetTriple = llvm::sys::getDefaultTargetTriple();
  m_module->setTargetTriple(targetTriple);

  std::string targetError;
  const llvm::Target *target =
      llvm::TargetRegistry::lookupTarget(targetTriple, targetError);
  if (!target) {
    setError(targetError);
    return false;
  }

  llvm::TargetOptions options;
#if LLVM_VERSION_MAJOR >= 16
  std::optional<llvm::Reloc::Model> relocModel;
#else
  llvm::Optional<llvm::Reloc::Model> relocModel;
#endif
  auto targetMachine =
      std::unique_ptr<llvm::TargetMachine>(target->createTargetMachine(
          targetTriple, "generic", "", options, relocModel));

  if (!targetMachine) {
    setError("Failed to create LLVM target machine.");
    return false;
  }

  m_module->setDataLayout(targetMachine->createDataLayout());

  std::error_code ec;
  llvm::raw_fd_ostream dest(path, ec, llvm::sys::fs::OF_None);
  if (ec) {
    setError(ec.message());
    return false;
  }

  llvm::legacy::PassManager pass;
  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr,
                                         llvm::CGFT_ObjectFile)) {
    setError("LLVM target does not support object file emission.");
    return false;
  }

  pass.run(*m_module);
  dest.flush();
  return true;
}

bool blaze::backend::LLVMBackend::emitExecutable(
    const std::string &outputPath, const std::vector<std::string> &objectFiles,
    const std::string &linkerPath, const std::vector<std::string> &linkerArgs,
    std::string *error) const {
  auto setError = [&](const std::string &message) {
    if (error)
      *error = message;
  };

  if (linkerPath.empty()) {
    setError("No linker path provided.");
    return false;
  }

  if (objectFiles.empty()) {
    setError("No object files provided.");
    return false;
  }

  std::vector<std::string> argsStorage;
  argsStorage.reserve(linkerArgs.size() + objectFiles.size() + 3);
  argsStorage.push_back(linkerPath);
  for (const auto &arg : linkerArgs)
    argsStorage.push_back(arg);
  argsStorage.push_back("-o");
  argsStorage.push_back(outputPath);
  for (const auto &obj : objectFiles)
    argsStorage.push_back(obj);

  std::vector<llvm::StringRef> args;
  args.reserve(argsStorage.size());
  for (const auto &arg : argsStorage)
    args.push_back(arg);

  int result = llvm::sys::ExecuteAndWait(linkerPath, args);
  if (result != 0) {
    setError("Linker failed with exit code " + std::to_string(result) + ".");
    return false;
  }

  return true;
}

llvm::Function *
blaze::backend::LLVMBackend::declareFunction(const frontend::IRFunction &func) {
  auto *funcType = llvm::FunctionType::get(llvmType(func.returnType),
                                           llvmTypes(func.parameters), false);

  auto *llvmFunc = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, func.name, m_module.get());

  auto argIt = llvmFunc->arg_begin();
  for (size_t i = 0; i < func.parameters.size() && argIt != llvmFunc->arg_end();
       ++i, ++argIt) {
    const auto &param = func.parameters[i];
    if (!param.name.empty())
      argIt->setName(param.name);
    else
      argIt->setName("arg" + std::to_string(i));
  }

  return llvmFunc;
}

void blaze::backend::LLVMBackend::emitBlocks(const frontend::IRFunction &func,
                                             llvm::Function *llvmFunc) {
  m_blockMap.clear();
  m_regValues.clear();

  for (const auto &block : func.blocks) {
    const auto name = block.id.id == 0 ? std::string("entry")
                                       : "bb" + std::to_string(block.id.id);
    auto *bb = llvm::BasicBlock::Create(*m_context, name, llvmFunc);
    m_blockMap.emplace(block.id.id, bb);
  }

  auto argIt = llvmFunc->arg_begin();
  for (size_t i = 0; i < func.parameters.size() && argIt != llvmFunc->arg_end();
       ++i, ++argIt) {
    m_regValues[func.parameters[i].reg.id] = &*argIt;
  }

  struct PhiInfo {
    llvm::PHINode *node = nullptr;
    std::vector<std::pair<frontend::BlockId, frontend::Register>> sources;
    core::SourceLocation loc;
  };
  std::vector<PhiInfo> phiInfos;

  for (const auto &block : func.blocks) {
    auto *bb = m_blockMap.at(block.id.id);
    m_builder->SetInsertPoint(bb, bb->begin());
    for (const auto &instr : block.instructions) {
      if (!std::holds_alternative<frontend::PhiInstruction>(instr))
        continue;
      const auto &phiInstr = std::get<frontend::PhiInstruction>(instr);
      auto *phi = m_builder->CreatePHI(llvmType(phiInstr.dest.type),
                                       phiInstr.sources.size());
      m_regValues[phiInstr.dest.id] = phi;
      phiInfos.push_back(PhiInfo{phi, phiInstr.sources, instr.location});
    }
  }

  for (const auto &block : func.blocks) {
    auto *bb = m_blockMap.at(block.id.id);

    m_builder->SetInsertPoint(bb);
    for (const auto &instr : block.instructions) {
      if (std::holds_alternative<frontend::PhiInstruction>(instr))
        continue;
      emitInstruction(instr);
    }

    if (block.terminator.has_value()) {
      emitTerminator(*block.terminator, llvmFunc);
    } else {
      m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                   "Missing terminator in IR block.",
                                   core::SourceLocation::empty());
    }
  }

  for (const auto &phiInfo : phiInfos) {
    for (const auto &[blockId, reg] : phiInfo.sources) {
      auto predIt = m_blockMap.find(blockId.id);
      if (predIt == m_blockMap.end()) {
        m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                     "PHI source block not found.",
                                     phiInfo.loc);
        continue;
      }

      auto valIt = m_regValues.find(reg.id);
      if (valIt == m_regValues.end()) {
        m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                     "Use of undefined register %" +
                                         std::to_string(reg.id) + ".",
                                     phiInfo.loc);
        phiInfo.node->addIncoming(llvm::UndefValue::get(llvmType(reg.type)),
                                  predIt->second);
        continue;
      }
      phiInfo.node->addIncoming(valIt->second, predIt->second);
    }
  }
}

void blaze::backend::LLVMBackend::emitInstruction(
    const frontend::Instruction &instr) {

  // Ghost instructions encode verification obligations and assumptions.
  // They have no runtime semantics — skip them entirely, except for
  // HavocInstruction which must still produce an SSA placeholder.
  if (instr.isGhost &&
      !std::holds_alternative<frontend::HavocInstruction>(instr)) {
    return;
  }

  if (std::holds_alternative<frontend::BinaryInstruction>(instr)) {
    const auto &binaryInstr = std::get<frontend::BinaryInstruction>(instr);
    llvm::Value *lhs = valueOf(binaryInstr.lhs, instr.location);
    llvm::Value *rhs = valueOf(binaryInstr.rhs, instr.location);
    llvm::Value *result = nullptr;

    switch (binaryInstr.op) {
    case frontend::BinaryOperation::Addition:
      result = m_builder->CreateAdd(lhs, rhs);
      break;
    case frontend::BinaryOperation::Subtraction:
      result = m_builder->CreateSub(lhs, rhs);
      break;
    case frontend::BinaryOperation::Multiplication:
      result = m_builder->CreateMul(lhs, rhs);
      break;
    case frontend::BinaryOperation::Division:
      result = m_builder->CreateSDiv(lhs, rhs);
      break;
    case frontend::BinaryOperation::LessThan:
      result = m_builder->CreateICmpSLT(lhs, rhs);
      break;
    case frontend::BinaryOperation::LessEqual:
      result = m_builder->CreateICmpSLE(lhs, rhs);
      break;
    case frontend::BinaryOperation::GreaterThan:
      result = m_builder->CreateICmpSGT(lhs, rhs);
      break;
    case frontend::BinaryOperation::GreaterEqual:
      result = m_builder->CreateICmpSGE(lhs, rhs);
      break;
    case frontend::BinaryOperation::Equal:
      result = m_builder->CreateICmpEQ(lhs, rhs);
      break;
    case frontend::BinaryOperation::NotEqual:
      result = m_builder->CreateICmpNE(lhs, rhs);
      break;
    }

    if (!result) {
      result = llvm::UndefValue::get(llvmType(binaryInstr.dest.type));
    }
    if (binaryInstr.dest.type != frontend::IRType::Void) {
      m_regValues[binaryInstr.dest.id] = result;
    }
    return;
  }

  if (std::holds_alternative<frontend::UnaryInstruction>(instr)) {
    const auto &unaryInstr = std::get<frontend::UnaryInstruction>(instr);
    llvm::Value *src = valueOf(unaryInstr.src, instr.location);
    if (!src->getType()->isIntegerTy(1)) {
      m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                   "Unary NOT expects a boolean operand.",
                                   instr.location);
      if (unaryInstr.dest.type != frontend::IRType::Void) {
        m_regValues[unaryInstr.dest.id] =
            llvm::UndefValue::get(llvmType(unaryInstr.dest.type));
      }
      return;
    }
    llvm::Value *result = m_builder->CreateNot(src);
    if (unaryInstr.dest.type != frontend::IRType::Void) {
      m_regValues[unaryInstr.dest.id] = result;
    }
    return;
  }

  if (std::holds_alternative<frontend::CallInstruction>(instr)) {
    const auto &callInstr = std::get<frontend::CallInstruction>(instr);
    std::vector<llvm::Value *> args;
    std::vector<llvm::Type *> argTypes;
    args.reserve(callInstr.args.size());
    argTypes.reserve(callInstr.args.size());
    for (const auto &arg : callInstr.args) {
      args.push_back(valueOf(arg, instr.location));
      argTypes.push_back(llvmType(frontend::irTypeOf(arg)));
    }

    llvm::Function *callee = m_module->getFunction(callInstr.func.name);
    if (!callee) {
      // Callee was not declared in this module (e.g. an external function).
      // Create a forward declaration from the call-site types.
      auto *calleeType = llvm::FunctionType::get(llvmType(callInstr.dest.type),
                                                 argTypes, false);
      callee =
          llvm::Function::Create(calleeType, llvm::Function::ExternalLinkage,
                                 callInstr.func.name, m_module.get());
    }

    auto *call = m_builder->CreateCall(callee, args);
    if (callInstr.dest.type != frontend::IRType::Void) {
      m_regValues[callInstr.dest.id] = call;
    }
    return;
  }

  if (std::holds_alternative<frontend::AssignmentInstruction>(instr)) {
    const auto &assignInstr = std::get<frontend::AssignmentInstruction>(instr);
    if (assignInstr.dest.type != frontend::IRType::Void) {
      m_regValues[assignInstr.dest.id] =
          valueOf(assignInstr.src, instr.location);
    }
    return;
  }

  // HavocInstruction exists to tell the verification (VC) backend that a
  // register's value is unknown (e.g. after a loop body that may modify it).
  // In SSA lowering, bind the destination to a frozen undefined value so any
  // subsequent use has a well-formed SSA operand with a stable, arbitrary
  // value.
  if (std::holds_alternative<frontend::HavocInstruction>(instr)) {
    const auto &havocInstr = std::get<frontend::HavocInstruction>(instr);
    if (havocInstr.dest.type != frontend::IRType::Void) {
      auto *undefValue = llvm::UndefValue::get(llvmType(havocInstr.dest.type));
      m_regValues[havocInstr.dest.id] = m_builder->CreateFreeze(undefValue);
    }
    return;
  }

  // ContractCheckInstruction and ContractAssumeInstruction are ghost code
  // emitted exclusively for the verification backend. They encode
  // precondition/postcondition/invariant obligations and assumptions and
  // have no runtime semantics — the code generation backend ignores them.
  if (std::holds_alternative<frontend::ContractCheckInstruction>(instr)) {
    return;
  }

  if (std::holds_alternative<frontend::ContractAssumeInstruction>(instr)) {
    return;
  }

  m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                               "Unhandled instruction variant in LLVM backend.",
                               instr.location);
}

void blaze::backend::LLVMBackend::emitTerminator(
    const frontend::Terminator &term, llvm::Function *llvmFunc) {
  if (std::holds_alternative<frontend::ReturnTerminator>(term)) {
    const auto &ret = std::get<frontend::ReturnTerminator>(term);
    if (ret.returnValue.has_value()) {
      m_builder->CreateRet(valueOf(*ret.returnValue, term.location));
    } else {
      if (llvmFunc->getReturnType()->isVoidTy()) {
        m_builder->CreateRetVoid();
      } else {
        m_builder->CreateRet(llvm::UndefValue::get(llvmFunc->getReturnType()));
      }
    }
    return;
  }

  if (std::holds_alternative<frontend::BranchTerminator>(term)) {
    const auto &br = std::get<frontend::BranchTerminator>(term);
    llvm::Value *cond = valueOf(frontend::Operand{br.condition}, term.location);
    if (cond->getType()->isIntegerTy() &&
        cond->getType()->getIntegerBitWidth() != 1) {
      cond = m_builder->CreateICmpNE(
          cond, llvm::ConstantInt::get(cond->getType(), 0));
    }

    auto trueIt = m_blockMap.find(br.consequent.id);
    auto falseIt = m_blockMap.find(br.alternative.id);
    if (trueIt == m_blockMap.end() || falseIt == m_blockMap.end()) {
      m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                   "Branch target block not found.",
                                   term.location);
      return;
    }

    m_builder->CreateCondBr(cond, trueIt->second, falseIt->second);
    return;
  }

  if (std::holds_alternative<frontend::JumpTerminator>(term)) {
    const auto &jmp = std::get<frontend::JumpTerminator>(term);
    auto it = m_blockMap.find(jmp.target.id);
    if (it == m_blockMap.end()) {
      m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                   "Jump target block not found.",
                                   term.location);
      return;
    }
    m_builder->CreateBr(it->second);
    return;
  }

  m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                               "Unhandled terminator variant in LLVM backend.",
                               term.location);
}

llvm::Value *
blaze::backend::LLVMBackend::valueOf(const frontend::Operand &op,
                                     const core::SourceLocation &loc) {
  if (std::holds_alternative<frontend::Register>(op)) {
    const auto &reg = std::get<frontend::Register>(op);
    auto it = m_regValues.find(reg.id);
    if (it != m_regValues.end()) {
      return it->second;
    }
    m_diagnostics.reportInternal(
        core::ERROR_INTERNAL_ERROR,
        "Use of undefined register %" + std::to_string(reg.id) + ".", loc);
    return llvm::UndefValue::get(llvmType(reg.type));
  }

  if (std::holds_alternative<frontend::IntConstant>(op)) {
    const auto &constant = std::get<frontend::IntConstant>(op);
    return llvm::ConstantInt::get(llvmType(frontend::IRType::I32),
                                  static_cast<uint64_t>(constant.value));
  }

  const auto &constant = std::get<frontend::BoolConstant>(op);
  return llvm::ConstantInt::get(llvmType(frontend::IRType::Bool),
                                constant.value);
}

llvm::Type *blaze::backend::LLVMBackend::llvmType(frontend::IRType type) {
  auto it = m_typeMap.find(type);
  if (it != m_typeMap.end()) {
    return it->second;
  }

  return llvm::Type::getVoidTy(*m_context);
}

std::vector<llvm::Type *> blaze::backend::LLVMBackend::llvmTypes(
    const std::vector<frontend::IRParam> &types) {
  std::vector<llvm::Type *> result;
  for (const auto &type : types)
    result.push_back(llvmType(type.reg.type));
  return result;
}
