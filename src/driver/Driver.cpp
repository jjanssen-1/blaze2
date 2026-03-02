#include "driver/Driver.h"

#include "backend/llvm/llvmbackend.h"
#include "backend/z3/z3backend.h"
#include "core/Errors.h"
#include "frontend/Builtins.h"
#include "frontend/FrontendDriver.h"
#include "frontend/IrLower.h"
#include "frontend/IrPrinter.h"
#include "frontend/SymbolTable.h"

#include <llvm/Support/Program.h>

#include <cstdio>
#include <sstream>

namespace blaze::driver {

DriverResult run(const CompileOptions &options) {
  DriverResult result;
  result.status = DriverResult::Ok;

  // 1 & 2. Load source, parse, resolve, typecheck
  auto checked = frontend::checkFile(options.inputFile, result.diagnostics);
  if (!checked) {
    result.status = DriverResult::CompileError;
    return result;
  }

  // 3. Lower to IR
  auto symbolTable =
      std::make_shared<frontend::SymbolTable>(checked->resolve.symbols);
  auto builtins =
      std::make_shared<frontend::BuiltinRegistry>(checked->resolve.builtins);

  frontend::IRBuilder builder(result.diagnostics);
  auto functions = builder.lower(checked->parse.root, symbolTable, builtins);

  if (result.diagnostics.hasErrors()) {
    result.status = DriverResult::CompileError;
    return result;
  }

  if (options.printIR || options.outputKind == OutputKind::PrintIR) {
    std::stringstream ss;
    for (const auto &func : functions) {
      ss << frontend::irPrint(func);
    }
    result.irText = ss.str();
  }

  // 4. Verification pass
  if (options.verify || options.verifyOnly) {
    backend::Z3Backend z3;
    auto verifyResult = z3.verify(functions);
    if (verifyResult.status != backend::VcBackend::Status::Verified) {
      for (const auto &cx : verifyResult.counterExamples) {
        std::string message = "Verification failed";
        if (cx.violatedPrecondition) {
          message += ": violated precondition of " +
                     cx.violatedPrecondition->functionName;
        }
        result.diagnostics.reportError(core::ERROR_VERIFICATION_FAILED, message,
                                       cx.checkLocation);
      }
      if (verifyResult.counterExamples.empty()) {
        result.diagnostics.reportError(core::ERROR_VERIFICATION_FAILED,
                                       "Verification failed",
                                       core::SourceLocation::empty());
      }
      result.status = DriverResult::VerificationFailed;
      return result;
    }
  }

  if (options.verifyOnly || options.outputKind == OutputKind::PrintIR) {
    return result;
  }

  // 5. Codegen pass
  backend::LLVMBackend llvmBackend(result.diagnostics);
  auto status = llvmBackend.generate(functions);
  if (status != backend::CodeBackend::Success ||
      result.diagnostics.hasErrors()) {
    result.status = DriverResult::CompileError;
    return result;
  }

  // Derive output path if empty
  std::string outputPath = options.outputFile;
  if (outputPath.empty()) {
    std::string out = options.inputFile;
    size_t lastDot = out.find_last_of('.');
    if (lastDot != std::string::npos) {
      out = out.substr(0, lastDot);
    }

    if (options.outputKind == OutputKind::EmitLLVM) {
      outputPath = out + ".ll";
    } else if (options.outputKind == OutputKind::EmitObject) {
      outputPath = out + ".o";
    } else if (options.outputKind == OutputKind::EmitExecutable) {
      outputPath = out;
    }
  }

  // Emit outputs
  std::string error;
  bool emitSuccess = false;

  if (options.outputKind == OutputKind::EmitLLVM) {
    emitSuccess = llvmBackend.emitIRToFile(outputPath, &error);
  } else if (options.outputKind == OutputKind::EmitObject) {
    emitSuccess = llvmBackend.emitObjectFile(outputPath, &error);
  } else if (options.outputKind == OutputKind::EmitExecutable) {
    std::string tempObject = outputPath + ".o";
    if (!llvmBackend.emitObjectFile(tempObject, &error)) {
      emitSuccess = false;
    } else {
      std::string linker = options.linkerPath;
      if (linker.empty()) {
        std::vector<llvm::StringRef> linkers = {"cc", "gcc", "clang"};
        for (auto l : linkers) {
          if (auto path = llvm::sys::findProgramByName(l)) {
            linker = *path;
            break;
          }
        }
        if (linker.empty()) {
          linker = "cc";
        }
      }
      emitSuccess = llvmBackend.emitExecutable(outputPath, {tempObject}, linker,
                                               options.linkerArgs, &error);
      std::remove(tempObject.c_str());
    }
  }

  if (!emitSuccess) {
    result.diagnostics.reportError(core::ERROR_INTERNAL_ERROR,
                                   "Failed to emit output: " + error,
                                   core::SourceLocation::empty());
    result.status = DriverResult::InternalError;
  }

  return result;
}

} // namespace blaze::driver
