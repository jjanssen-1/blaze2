#include "TypeChecker.h"

#include "frontend/Ast.h"

#include "core/Errors.h"

#include <variant>

namespace blaze::frontend {

bool TypeChecker::expectType(const SymbolId &expected, const SymbolId &actual) {
  // Sufficient for now. Later maybe add indirection resolution to allow
  // implicit conversions.
  return expected == actual;
}

void TypeChecker::checkFunctionSpecs(const FunctionSpecifications &spec) {
  SymbolId boolType = m_builtins.findType("bool")->symbolId;
  for (const auto &expr : spec.pre) {
    auto type = getExprType(expr);
    if (type.has_value() && type.value() != boolType) {
      reportTypeMismatch(boolType, type.value());
    }
  }

  for (const auto &expr : spec.post) {
    auto type = getExprType(expr);
    if (type.has_value() && type.value() != boolType) {
      reportTypeMismatch(boolType, type.value());
    }
  }
}

void TypeChecker::checkFunction(const Function &function) {
  checkFunctionSpecs(function.specifications);

  // Set the current return type so return statements can be checked against it.
  if (function.returnType.has_value() &&
      function.returnType->identifier.symbolId.has_value()) {
    m_currentReturnType = function.returnType->identifier.symbolId;
  } else {
    // Functions without an explicit return type default to void.
    const auto *voidEntry = m_builtins.findType("void");
    m_currentReturnType =
        voidEntry ? std::optional<SymbolId>(voidEntry->symbolId) : std::nullopt;
  }

  checkBlock(function.body);

  m_currentReturnType = std::nullopt;
}

void TypeChecker::checkBlock(const Block &block) {
  for (const auto &statement : block.statements) {
    checkStatement(statement);
  }
}

void TypeChecker::checkStatement(const Statement &stmt) {
  if (std::holds_alternative<DeclStmt>(stmt)) {
    checkDeclaration(std::get<DeclStmt>(stmt));
  } else if (std::holds_alternative<ReturnStmt>(stmt)) {
    const auto &retStmt = std::get<ReturnStmt>(stmt);

    if (!m_currentReturnType.has_value()) {
      m_diagnostics.reportError(core::ERROR_ILLEGAL_STATEMENT,
                                "Return statement outside of function");
      return;
    }

    if (!retStmt.expression.has_value()) {
      // `return;` with no expression — only valid if the function returns void.
      if (!isVoid(*m_currentReturnType)) {
        // Expected a return value but got none.  Report using void as the
        // "actual" type so the message is meaningful.
        const auto *voidEntry = m_builtins.findType("void");
        if (voidEntry) {
          reportTypeMismatch(*m_currentReturnType, voidEntry->symbolId);
        }
      }
      return;
    }

    // Expression is present — its type must match the function's return type.
    auto type = getExprType(retStmt.expression.value());
    if (type.has_value() && !expectType(*m_currentReturnType, type.value())) {
      reportTypeMismatch(*m_currentReturnType, type.value());
    }
  } else if (std::holds_alternative<IfStmt>(stmt)) {
    const IfStmt &statement = std::get<IfStmt>(stmt);
    // Check if the condition is a boolean expression
    auto conditionType = getExprType(statement.condition);
    if (!conditionType.has_value() || !isBoolean(*conditionType)) {
      m_diagnostics.reportError(core::ERROR_TYPE_MISMATCH,
                                "Condition must be a boolean expression");
      m_error = true;
    }

    checkStatement(*statement.consequent);
    if (statement.alternative) {
      // Optional<Ptr>
      checkStatement(**statement.alternative);
    }
  } else if (std::holds_alternative<WhileStmt>(stmt)) {
    const WhileStmt &statement = std::get<WhileStmt>(stmt);
    // Check if the condition is a boolean expression
    auto conditionType = getExprType(statement.condition);

    if (!conditionType.has_value() || !isBoolean(*conditionType)) {
      m_diagnostics.reportError(core::ERROR_TYPE_MISMATCH,
                                "Condition must be a boolean expression");
      m_error = true;
    }
    checkStatement(*statement.body);
  } else if (std::holds_alternative<ExprPtr>(stmt)) {
    const ExprPtr &expr = std::get<ExprPtr>(stmt);
    checkExpression(expr);
  } else if (std::holds_alternative<BlockPtr>(stmt)) {
    const BlockPtr &block = std::get<BlockPtr>(stmt);
    checkBlock(*block);
  }
}

void TypeChecker::checkDeclaration(const DeclStmt &decl) {
  checkExpression(decl.assignedExpression);

  // Check if assigned expression matches the declared type
  auto declType = decl.type.identifier.symbolId;
  auto exprType = getExprType(decl.assignedExpression);
  if (declType && exprType && !expectType(*declType, *exprType)) {
    reportTypeMismatch(*declType, *exprType);
  }
}

bool TypeChecker::isVoid(const SymbolId &type) {
  const auto *entry = m_builtins.findType("void");
  return entry && type == entry->symbolId;
}

bool TypeChecker::isBoolean(const SymbolId &type) {
  const auto *entry = m_builtins.findType("bool");
  return entry && type == entry->symbolId;
}

void TypeChecker::checkExpression(const ExprPtr &expr) {
  if (!expr) {
    m_diagnostics.reportInternal(core::ERROR_INTERNAL_ERROR,
                                 "Tried to check a null expression");
    return;
  }
  if (std::holds_alternative<BinaryExpr>(*expr)) {
    const auto &binaryExpr = std::get<BinaryExpr>(*expr);
    checkExpression(binaryExpr.left);
    checkExpression(binaryExpr.right);

    // Both operands must have the same type.
    auto leftType = getExprType(binaryExpr.left);
    auto rightType = getExprType(binaryExpr.right);
    if (leftType.has_value() && rightType.has_value() &&
        !expectType(*leftType, *rightType)) {
      reportTypeMismatch(*leftType, *rightType);
    }
  } else if (std::holds_alternative<UnaryExpr>(*expr)) {
    const auto &unaryExpr = std::get<UnaryExpr>(*expr);
    checkExpression(unaryExpr.operand);
  } else if (std::holds_alternative<CallExpr>(*expr)) {
    const auto &callExpr = std::get<CallExpr>(*expr);
    for (const auto &arg : callExpr.arguments) {
      checkExpression(arg);
    }
  }

  // Var, int, bool expressions can be ignored.
}

void TypeChecker::reportTypeMismatch(const SymbolId &expected,
                                     const SymbolId &actual) {
  m_error = true;
  m_diagnostics.reportError(
      core::ERROR_TYPE_MISMATCH,
      "Type mismatch: expected '" + std::string(typeName(expected)) +
          "' but got '" + std::string(typeName(actual)) + "'");
}

std::string_view TypeChecker::typeName(const SymbolId &type) const {
  const auto &symbol = m_table.get(type);
  auto name = symbol.name.name();
  return name.empty() ? "<unknown>" : name;
}

} // namespace blaze::frontend
