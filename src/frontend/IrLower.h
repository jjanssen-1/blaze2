#pragma once

#include "Builtins.h"
#include "Ir.h"
#include "core/Errors.h"
#include "frontend/Ast.h"
#include "frontend/SymbolTable.h"

#include "core/Source.h"

#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace blaze::frontend {

class IRBuilder {
public:
  explicit IRBuilder(core::DiagnosticList &diagnostics);

  std::vector<IRFunction>
  lower(const std::shared_ptr<Root> &root,
        std::shared_ptr<SymbolTable> symbolTable,
        std::shared_ptr<BuiltinRegistry> builtinRegistry);

private:
  bool m_isGhost = false;
  const FunctionSpecifications *m_currentSpecs = nullptr;
  core::u64 m_nextBlockId;
  core::u64 m_nextRegisterId;
  std::vector<IRBlock> m_blocks;
  core::u64 m_currentBlockIdx;
  std::unordered_map<core::u64, Register> m_symbolToRegister;
  std::unordered_map<core::u64, core::SourceLocation> m_registerLocations;

  // Tracks which symbols (by raw SymbolId) have been definitely assigned
  // a value. Used for use-before-initialization diagnostics.
  std::unordered_set<core::u64> m_initializedSymbols;

  std::shared_ptr<SymbolTable> m_currentSymbolTable;
  std::shared_ptr<BuiltinRegistry> m_builtinRegistry;
  core::DiagnosticList &m_diagnostics;

  void resetFunctionState();
  Register allocateRegister(IRType type, const core::SourceLocation &loc);
  IRType resolveIRType(const SymbolId &symbolId);
  IRType resultTypeOfBinaryOp(BinaryOperation op, IRType operandType);
  BlockId createBlock();
  IRBlock &currentBlock();
  void setCurrentBlock(BlockId block);
  void emit(const core::SourceLocation &loc, bool isGhost,
            Instruction::Variant &&inst);
  void emit(const core::SourceLocation &loc, Instruction::Variant &&inst);
  void terminate(const core::SourceLocation &loc, Terminator::Variant &&term);

  IRFunction lowerFunction(const Function &func);
  void lowerStatement(const Statement &stmt);
  void lowerBlock(const Block &block);
  void lowerDeclStmt(const DeclStmt &stmt);
  void lowerReturnStmt(const ReturnStmt &stmt);
  void lowerIfStmt(const IfStmt &stmt);
  void lowerWhileStmt(const WhileStmt &stmt);
  void lowerAssignmentStmt(const AssignmentStmt &stmt);
  Operand lowerExpression(const ExprPtr &expr);
  Operand lowerBinaryExpr(const core::SourceLocation &loc,
                          const BinaryExpr &expr);
  Operand lowerUnaryExpr(const core::SourceLocation &loc,
                         const UnaryExpr &expr);
  Operand lowerCallExpr(const core::SourceLocation &loc, const CallExpr &expr);
  Operand lowerVarExpr(const core::SourceLocation &loc, const VarExpr &expr);
  Operand lowerIntExpr(const IntExpr &expr);
  Operand lowerBoolExpr(const BoolExpr &expr);
  void lowerContractAssumption(const std::vector<ExprPtr> &assumptions);
  void lowerContractChecks(const std::vector<ExprPtr> &assertions);

  bool isVoid(const SymbolId &symbolId);
};

} // namespace blaze::frontend
