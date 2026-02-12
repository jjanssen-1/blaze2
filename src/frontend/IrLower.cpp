#include "IrLower.h"

#include "Ir.h"
#include "core/Errors.h"
#include "core/Source.h"
#include "frontend/Ast.h"
#include "frontend/Builtins.h"

#include <cassert>
#include <memory>

namespace blaze::frontend {

IRBuilder::IRBuilder(core::DiagnosticList &diagnostics)
    : m_isGhost(false), m_nextBlockId(0), m_nextRegisterId(0),
      m_currentBlockIdx(0), m_currentSymbolTable(nullptr),
      m_diagnostics(diagnostics) {}

std::vector<IRFunction>
IRBuilder::lower(const std::shared_ptr<Root> &root,
                 std::shared_ptr<SymbolTable> symbolTable,
                 std::shared_ptr<BuiltinRegistry> builtinRegistry) {
  m_currentSymbolTable = symbolTable;
  m_builtinRegistry = builtinRegistry;
  std::vector<IRFunction> functions;
  functions.reserve(root->functions.size());
  for (const auto &func : root->functions) {
    functions.push_back(lowerFunction(func));
  }
  return functions;
}

void IRBuilder::resetFunctionState() {
  m_isGhost = false;
  m_currentSpecs = nullptr;
  m_nextBlockId = 0;
  m_nextRegisterId = 0;
  m_blocks.clear();
  m_currentBlockIdx = 0;
  m_symbolToRegister.clear();
  m_initializedSymbols.clear();
  m_registerLocations.clear();
}

Register IRBuilder::allocateRegister(IRType type,
                                     const core::SourceLocation &loc) {
  Register reg{m_nextRegisterId++, type};
  m_registerLocations.emplace(reg.id, loc);
  return reg;
}

IRType IRBuilder::resolveIRType(const SymbolId &symbolId) {
  const auto *i32Entry = m_builtinRegistry->findType("i32");
  if (i32Entry && symbolId == i32Entry->symbolId)
    return IRType::I32;
  const auto *boolEntry = m_builtinRegistry->findType("bool");
  if (boolEntry && symbolId == boolEntry->symbolId)
    return IRType::Bool;
  return IRType::Void;
}

IRType IRBuilder::resultTypeOfBinaryOp(BinaryOperation op, IRType operandType) {
  switch (op) {
  case BinaryOperation::LessThan:
  case BinaryOperation::LessEqual:
  case BinaryOperation::GreaterThan:
  case BinaryOperation::GreaterEqual:
  case BinaryOperation::Equal:
  case BinaryOperation::NotEqual:
    return IRType::Bool;
  default:
    return operandType;
  }
}

BlockId IRBuilder::createBlock() {
  BlockId id{m_nextBlockId++};
  m_blocks.push_back(IRBlock{id, {}, std::nullopt});
  return id;
}

IRBlock &IRBuilder::currentBlock() { return m_blocks[m_currentBlockIdx]; }

void IRBuilder::setCurrentBlock(BlockId block) {
  if (block.id >= m_blocks.size()) {
    m_diagnostics.reportInfo(core::ERROR_INTERNAL_ERROR,
                             "Tried setting current block to invalid block",
                             core::SourceLocation::empty());
    assert(false);
  }
  m_currentBlockIdx = block.id;
}

void IRBuilder::emit(const core::SourceLocation &loc, bool isGhost,
                     Instruction::Variant &&inst) {
  currentBlock().instructions.emplace_back(loc, isGhost, std::move(inst));
}

void IRBuilder::emit(const core::SourceLocation &loc,
                     Instruction::Variant &&inst) {
  emit(loc, m_isGhost, std::move(inst));
}

void IRBuilder::terminate(const core::SourceLocation &loc,
                          Terminator::Variant &&term) {
  if (currentBlock().terminator.has_value()) {
    m_diagnostics.reportError(
        core::ERROR_INTERNAL_ERROR,
        "Trying to terminate a block that already has a terminator",
        core::SourceLocation::empty());
    assert(false);
  }
  currentBlock().terminator.emplace(loc, std::move(term));
}

IRFunction IRBuilder::lowerFunction(const Function &func) {
  resetFunctionState();

  // Resolve the function's return type.
  IRType irReturnType = IRType::Void;
  if (func.returnType.has_value() &&
      func.returnType->identifier.symbolId.has_value()) {
    irReturnType = resolveIRType(*func.returnType->identifier.symbolId);
  }

  // Create the entry block (always BlockId{0}).
  BlockId entryBlock = createBlock();
  setCurrentBlock(entryBlock);

  // Parameters are always initialized
  std::vector<IRParam> irParams;
  irParams.reserve(func.parameters.size());
  for (const auto &param : func.parameters) {
    IRType paramType = IRType::Void;
    if (param.type.identifier.symbolId.has_value()) {
      paramType = resolveIRType(*param.type.identifier.symbolId);
    }
    Register reg = allocateRegister(paramType, param.location);
    irParams.push_back(IRParam{reg, param.identifier.name});
    if (param.identifier.symbolId.has_value()) {
      auto raw = param.identifier.symbolId->raw();
      m_symbolToRegister[raw] = reg;
      m_initializedSymbols.insert(raw);
    }
  }

  // Store the current function's specifications so that lowerReturnStmt
  // can access postconditions at each return site.
  m_currentSpecs = &func.specifications;

  // Take preconditions as facts (assumptions) at function entry.
  lowerContractAssumption(func.specifications.pre);

  // Lower the function body.
  lowerBlock(func.body);

  if (!currentBlock().terminator.has_value()) {
    if (!func.returnsVoid()) {
      m_diagnostics.reportError(core::ERROR_FUNCTION_NO_TERMINATOR,
                                "Function does not have a terminator",
                                func.location);
    }
    // Lower postcondition checks before the implicit void return.
    if (m_currentSpecs && !m_currentSpecs->post.empty()) {
      lowerContractChecks(m_currentSpecs->post);
    }
    terminate(func.body.location, ReturnTerminator{std::nullopt});
  }

  IRFunction irFunc;
  irFunc.name = func.identifier.name;
  irFunc.returnType = irReturnType;
  irFunc.parameters = std::move(irParams);
  irFunc.blocks = std::move(m_blocks);
  irFunc.registerLocations = std::move(m_registerLocations);
  return irFunc;
}

void IRBuilder::lowerStatement(const Statement &stmt) {
  if (std::holds_alternative<DeclStmt>(stmt)) {
    lowerDeclStmt(std::get<DeclStmt>(stmt));
  } else if (std::holds_alternative<ReturnStmt>(stmt)) {
    lowerReturnStmt(std::get<ReturnStmt>(stmt));
  } else if (std::holds_alternative<IfStmt>(stmt)) {
    lowerIfStmt(std::get<IfStmt>(stmt));
  } else if (std::holds_alternative<WhileStmt>(stmt)) {
    lowerWhileStmt(std::get<WhileStmt>(stmt));
  } else if (std::holds_alternative<AssignmentStmt>(stmt)) {
    lowerAssignmentStmt(std::get<AssignmentStmt>(stmt));
  } else if (std::holds_alternative<ExprPtr>(stmt)) {
    // Expression statement: lower for side effects, discard result.
    lowerExpression(std::get<ExprPtr>(stmt));
  } else if (std::holds_alternative<BlockPtr>(stmt)) {
    lowerBlock(*std::get<BlockPtr>(stmt));
  }
}

void IRBuilder::lowerBlock(const Block &block) {
  for (const auto &stmt : block.statements) {
    if (currentBlock().terminator.has_value()) {
      m_diagnostics.reportError(core::ERROR_UNREACHABLE_CODE,
                                "Detected unreachable code", stmt.location);
      break;
    }
    lowerStatement(stmt);
  }
}

void IRBuilder::lowerDeclStmt(const DeclStmt &stmt) {
  IRType declType = IRType::Void;
  if (stmt.type.identifier.symbolId.has_value()) {
    declType = resolveIRType(*stmt.type.identifier.symbolId);
  }
  Register dest = allocateRegister(declType, stmt.location);

  // Always bind the declared symbol to its register, even without
  // an initializer — later references need to find it.
  if (stmt.identifier.symbolId.has_value()) {
    m_symbolToRegister[stmt.identifier.symbolId->raw()] = dest;
  }

  if (stmt.assignedExpression) {
    Operand value = lowerExpression(stmt.assignedExpression);
    emit(stmt.location, AssignmentInstruction{dest, value});

    // Mark the symbol as definitely initialized.
    if (stmt.identifier.symbolId.has_value()) {
      m_initializedSymbols.insert(stmt.identifier.symbolId->raw());
    }
  }
  // If there is no initializer, the symbol is NOT added to
  // m_initializedSymbols, so any use before assignment will be caught.
}

void IRBuilder::lowerReturnStmt(const ReturnStmt &stmt) {
  if (stmt.expression.has_value()) {
    Operand value = lowerExpression(*stmt.expression);

    // Lower postcondition assertions
    if (m_currentSpecs && !m_currentSpecs->post.empty()) {
      // Bind the result identifier to the return value register.
      if (m_currentSpecs->postResultBinding.has_value() &&
          m_currentSpecs->postResultBinding->symbolId.has_value()) {
        Register retReg{0, IRType::Void};
        if (std::holds_alternative<Register>(value)) {
          retReg = std::get<Register>(value);
        } else {
          // Materialize the constant into a ghost register for the binding.
          retReg = allocateRegister(irTypeOf(value), stmt.location);
          emit(stmt.location, true, AssignmentInstruction{retReg, value});
        }
        auto raw = m_currentSpecs->postResultBinding->symbolId->raw();
        m_symbolToRegister[raw] = retReg;
        m_initializedSymbols.insert(raw);
      }
      lowerContractChecks(m_currentSpecs->post);
    }

    terminate(stmt.location, ReturnTerminator{value});
  } else {
    // Void return — postconditions without a result binding may still apply.
    if (m_currentSpecs && !m_currentSpecs->post.empty()) {
      lowerContractChecks(m_currentSpecs->post);
    }
    terminate(stmt.location, ReturnTerminator{std::nullopt});
  }
}

void IRBuilder::lowerIfStmt(const IfStmt &stmt) {
  Operand cond = lowerExpression(stmt.condition);

  // We need the condition in a register for the branch terminator.
  Register condReg{0, IRType::Bool};
  if (std::holds_alternative<Register>(cond)) {
    condReg = std::get<Register>(cond);
  } else {
    // Materialise the constant into a register.
    condReg = allocateRegister(IRType::Bool, stmt.location);
    emit(stmt.location, AssignmentInstruction{condReg, cond});
  }

  BlockId consequentBlock = createBlock();
  BlockId mergeBlock = createBlock();

  // Save the initialized-symbols state before branching. After both
  // branches execute, only symbols initialized on ALL paths are
  // considered definitely initialized (intersection).
  auto savedInitialized = m_initializedSymbols;

  if (stmt.alternative.has_value()) {
    BlockId alternativeBlock = createBlock();

    // Terminate current block with a conditional branch.
    terminate(stmt.location,
              BranchTerminator{condReg, consequentBlock, alternativeBlock});

    // consequent
    m_initializedSymbols = savedInitialized;
    setCurrentBlock(consequentBlock);
    lowerStatement(*stmt.consequent);
    bool thenTerminated = currentBlock().terminator.has_value();
    if (!thenTerminated) {
      terminate(stmt.location, JumpTerminator{mergeBlock});
    }
    auto thenInitialized = m_initializedSymbols;

    // alternative
    m_initializedSymbols = savedInitialized;
    setCurrentBlock(alternativeBlock);
    lowerStatement(**stmt.alternative);
    bool elseTerminated = currentBlock().terminator.has_value();
    if (!elseTerminated) {
      terminate(stmt.location, JumpTerminator{mergeBlock});
    }
    auto elseInitialized = m_initializedSymbols;

    // Merge: a symbol is definitely initialized only if BOTH branches
    // initialized it (intersection).
    m_initializedSymbols.clear();
    for (const auto &sym : thenInitialized) {
      if (elseInitialized.count(sym)) {
        m_initializedSymbols.insert(sym);
      }
    }

    // If both branches terminated (e.g. both returned), the merge block
    // is unreachable.
    if (thenTerminated && elseTerminated) {
      setCurrentBlock(mergeBlock);
      terminate(stmt.location, ReturnTerminator{std::nullopt});
    }
  } else {
    // No else: branch to then or straight to merge.
    terminate(stmt.location,
              BranchTerminator{condReg, consequentBlock, mergeBlock});

    m_initializedSymbols = savedInitialized;
    setCurrentBlock(consequentBlock);
    lowerStatement(*stmt.consequent);
    if (!currentBlock().terminator.has_value()) {
      terminate(stmt.location, JumpTerminator{mergeBlock});
    }

    // Without an else branch, the "no-else" path doesn't initialize
    // anything new. Intersection with savedInitialized = savedInitialized.
    m_initializedSymbols = savedInitialized;
  }

  setCurrentBlock(mergeBlock);
}

void IRBuilder::lowerWhileStmt(const WhileStmt &stmt) {
  BlockId headerBlock = createBlock();
  BlockId bodyBlock = createBlock();
  BlockId exitBlock = createBlock();

  // Jump from the current block into the loop header.
  terminate(stmt.location, JumpTerminator{headerBlock});

  // Save initialized state before the loop. The loop body might not
  // execute at all (condition could be false on the first check), so
  // conservatively nothing new is definitely initialized after the loop.
  auto savedInitialized = m_initializedSymbols;

  setCurrentBlock(headerBlock);
  Operand cond = lowerExpression(stmt.condition);

  Register condReg{0, IRType::Bool};
  if (std::holds_alternative<Register>(cond)) {
    condReg = std::get<Register>(cond);
  } else {
    condReg = allocateRegister(IRType::Bool, stmt.location);
    emit(stmt.location, AssignmentInstruction{condReg, cond});
  }

  terminate(stmt.location, BranchTerminator{condReg, bodyBlock, exitBlock});

  setCurrentBlock(bodyBlock);
  lowerStatement(*stmt.body);
  if (!currentBlock().terminator.has_value()) {
    // Loop back to header.
    terminate(stmt.location, JumpTerminator{headerBlock});
  }

  // Restore to pre-loop state: the body might never execute, so we
  // cannot assume anything it initializes is definitely initialized.
  m_initializedSymbols = savedInitialized;

  // Continue emitting after the loop.
  setCurrentBlock(exitBlock);
}

void IRBuilder::lowerAssignmentStmt(const AssignmentStmt &stmt) {
  Operand value = lowerExpression(stmt.value);

  // Determine the variable's type from the symbol table.
  IRType varType = IRType::Void;
  if (stmt.identifier.symbolId.has_value()) {
    const auto &sym = m_currentSymbolTable->get(*stmt.identifier.symbolId);
    if (const auto *varInfo = std::get_if<VariableInfo>(&sym.info)) {
      varType = resolveIRType(varInfo->type);
    } else if (const auto *paramInfo = std::get_if<ParameterInfo>(&sym.info)) {
      varType = resolveIRType(paramInfo->type);
    }
  }

  Register dest = allocateRegister(varType, stmt.location);

  // Re-bind the symbol to the new register (SSA-style: new definition).
  if (stmt.identifier.symbolId.has_value()) {
    auto raw = stmt.identifier.symbolId->raw();
    m_symbolToRegister[raw] = dest;
    m_initializedSymbols.insert(raw);
  }

  emit(stmt.location, AssignmentInstruction{dest, value});
}

Operand IRBuilder::lowerExpression(const ExprPtr &expr) {
  if (!expr) {
    m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                 "Null expression pointer",
                                 core::SourceLocation::empty());
    return IntConstant{0};
  }

  const auto &loc = expr->location;
  const auto &v = static_cast<const Expression::Variant &>(*expr);

  if (std::holds_alternative<BinaryExpr>(v)) {
    return lowerBinaryExpr(loc, std::get<BinaryExpr>(v));
  } else if (std::holds_alternative<UnaryExpr>(v)) {
    return lowerUnaryExpr(loc, std::get<UnaryExpr>(v));
  } else if (std::holds_alternative<CallExpr>(v)) {
    return lowerCallExpr(loc, std::get<CallExpr>(v));
  } else if (std::holds_alternative<VarExpr>(v)) {
    return lowerVarExpr(loc, std::get<VarExpr>(v));
  } else if (std::holds_alternative<IntExpr>(v)) {
    return lowerIntExpr(std::get<IntExpr>(v));
  } else if (std::holds_alternative<BoolExpr>(v)) {
    return lowerBoolExpr(std::get<BoolExpr>(v));
  }

  m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                               "Unhandled expression variant", loc);
  return IntConstant{0};
}

Operand IRBuilder::lowerBinaryExpr(const core::SourceLocation &loc,
                                   const BinaryExpr &expr) {
  Operand lhs = lowerExpression(expr.left);
  Operand rhs = lowerExpression(expr.right);

  IRType resultType = resultTypeOfBinaryOp(expr.operation, irTypeOf(lhs));
  Register dest = allocateRegister(resultType, loc);
  emit(loc, BinaryInstruction{expr.operation, dest, lhs, rhs});
  return dest;
}

Operand IRBuilder::lowerUnaryExpr(const core::SourceLocation &loc,
                                  const UnaryExpr &expr) {
  Operand src = lowerExpression(expr.operand);

  Register dest = allocateRegister(irTypeOf(src), loc);
  emit(loc, UnaryInstruction{expr.operation, dest, src});
  return dest;
}

Operand IRBuilder::lowerCallExpr(const core::SourceLocation &loc,
                                 const CallExpr &expr) {
  // Lower all argument expressions.
  std::vector<Operand> args;
  args.reserve(expr.arguments.size());
  for (const auto &arg : expr.arguments) {
    args.push_back(lowerExpression(arg));
  }

  // The callee is identified by a FunctionRef carrying the function's
  // human-readable name and resolved symbol ID.
  FunctionRef func{expr.identifier.name, 0};
  const FunctionInfo *calleeInfo = nullptr;
  if (expr.identifier.symbolId.has_value()) {
    func.symbolId = expr.identifier.symbolId->raw();
    const auto &sym = m_currentSymbolTable->get(*expr.identifier.symbolId);
    calleeInfo = std::get_if<FunctionInfo>(&sym.info);
  }

  // Temporarily bind callee parameter symbols to argument registers so
  // that the callee's contract expressions can be lowered in the caller's
  // register environment.

  // We save and restore existing bindings rather than simply erasing
  // them after lowering, because in recursive calls the callee's
  // parameter symbols are the same as the caller's — erasing would
  // destroy the caller's own bindings.
  struct SavedBinding {
    core::u64 raw;
    bool hadRegister;
    Register savedRegister;
    bool wasInitialized;
  };
  std::vector<SavedBinding> tempBindings;

  if (calleeInfo && (!calleeInfo->pre.empty() || !calleeInfo->post.empty())) {
    for (core::size i = 0;
         i < args.size() && i < calleeInfo->parameterSymbols.size(); ++i) {
      auto raw = calleeInfo->parameterSymbols[i].raw();

      // Save any existing binding for this symbol.
      SavedBinding saved;
      saved.raw = raw;
      auto regIt = m_symbolToRegister.find(raw);
      saved.hadRegister = (regIt != m_symbolToRegister.end());
      saved.savedRegister =
          saved.hadRegister ? regIt->second : Register{0, IRType::Void};
      saved.wasInitialized =
          (m_initializedSymbols.find(raw) != m_initializedSymbols.end());
      tempBindings.push_back(saved);

      Register argReg{0, IRType::Void};
      if (std::holds_alternative<Register>(args[i])) {
        argReg = std::get<Register>(args[i]);
      } else {
        // Materialize the constant into a ghost register for the binding.
        argReg = allocateRegister(irTypeOf(args[i]), loc);
        emit(loc, true, AssignmentInstruction{argReg, args[i]});
      }
      m_symbolToRegister[raw] = argReg;
      m_initializedSymbols.insert(raw);
    }

    // Prove callee preconditions (assert/check).
    lowerContractChecks(calleeInfo->pre);
  }

  // Determine the return type of the callee.
  IRType callReturnType = IRType::Void;
  if (calleeInfo) {
    callReturnType = resolveIRType(calleeInfo->returnType);
  }

  Register dest = allocateRegister(callReturnType, loc);
  emit(loc, CallInstruction{dest, func, std::move(args)});

  // Assume callee postconditions hold after the call.
  if (calleeInfo && !calleeInfo->post.empty()) {
    // Bind the callee's result identifier to the call destination register.
    if (calleeInfo->postResultBinding.has_value() &&
        calleeInfo->postResultBinding->symbolId.has_value()) {
      auto raw = calleeInfo->postResultBinding->symbolId->raw();

      // Save any existing binding for the result symbol.
      SavedBinding saved;
      saved.raw = raw;
      auto regIt = m_symbolToRegister.find(raw);
      saved.hadRegister = (regIt != m_symbolToRegister.end());
      saved.savedRegister =
          saved.hadRegister ? regIt->second : Register{0, IRType::Void};
      saved.wasInitialized =
          (m_initializedSymbols.find(raw) != m_initializedSymbols.end());
      tempBindings.push_back(saved);

      m_symbolToRegister[raw] = dest;
      m_initializedSymbols.insert(raw);
    }
    lowerContractAssumption(calleeInfo->post);
  }

  // Restore previous bindings so callee symbols don't leak into the
  // caller's environment. For recursive calls this correctly restores
  // the caller's own parameter bindings.
  for (const auto &saved : tempBindings) {
    if (saved.hadRegister) {
      m_symbolToRegister[saved.raw] = saved.savedRegister;
    } else {
      m_symbolToRegister.erase(saved.raw);
    }
    if (saved.wasInitialized) {
      m_initializedSymbols.insert(saved.raw);
    } else {
      m_initializedSymbols.erase(saved.raw);
    }
  }

  return dest;
}

Operand IRBuilder::lowerVarExpr(const core::SourceLocation &loc,
                                const VarExpr &expr) {
  if (expr.identifier.symbolId.has_value()) {
    auto raw = expr.identifier.symbolId->raw();

    // Check definite-assignment: the symbol must have been initialized
    // before it can be referenced.
    if (m_initializedSymbols.find(raw) == m_initializedSymbols.end()) {
      m_diagnostics.reportError(core::ERROR_USE_BEFORE_INIT,
                                "Variable '" + expr.identifier.name +
                                    "' is used before being initialized",
                                loc);
    }

    auto it = m_symbolToRegister.find(raw);
    if (it != m_symbolToRegister.end()) {
      return it->second;
    }
  }

  m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                               "Expression was not resolved", loc);
  return Register{0, IRType::Void};
}

Operand IRBuilder::lowerIntExpr(const IntExpr &expr) {
  return IntConstant{expr.value};
}

Operand IRBuilder::lowerBoolExpr(const BoolExpr &expr) {
  return BoolConstant{expr.value};
}

bool IRBuilder::isVoid(const SymbolId &symbolId) {
  SymbolId voidSymbol = m_builtinRegistry->findType("void")->symbolId;
  return symbolId == voidSymbol;
}
void IRBuilder::lowerContractAssumption(
    const std::vector<ExprPtr> &assumptions) {
  bool savedGhost = m_isGhost;
  m_isGhost = true;
  for (const auto &assumption : assumptions) {
    Operand result = lowerExpression(assumption);
    emit(assumption->location, ContractAssumeInstruction{result});
  }
  m_isGhost = savedGhost;
}
void IRBuilder::lowerContractChecks(const std::vector<ExprPtr> &checks) {
  bool savedGhost = m_isGhost;
  m_isGhost = true;
  for (const auto &check : checks) {
    Operand result = lowerExpression(check);
    emit(check->location, ContractCheckInstruction{result});
  }
  m_isGhost = savedGhost;
}
} // namespace blaze::frontend
