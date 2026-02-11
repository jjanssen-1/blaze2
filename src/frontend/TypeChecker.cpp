#include "TypeChecker.h"

#include "core/Source.h"
#include "frontend/Ast.h"

#include "core/Errors.h"

#include <variant>

namespace blaze::frontend {

bool TypeChecker::expectType(const SymbolId &expected, const SymbolId &actual) {
  // Sufficient for now. Later maybe add indirection resolution to allow
  // implicit conversions.
  return expected == actual;
}

void TypeChecker::checkFunctionSpecs(const FunctionSpecifications &spec,
                                     bool isVoidFunction) {
  SymbolId boolType = m_builtins.findType("bool")->symbolId;
  for (const auto &expr : spec.pre) {
    auto type = getExprType(expr);
    if (type.has_value() && type.value() != boolType) {
      reportTypeMismatch(boolType, type.value(), expr->location);
    }
  }

  // Diagnose result binding on void functions.
  if (spec.postResultBinding.has_value() && isVoidFunction) {
    m_error = true;
    m_diagnostics.reportError(core::ERROR_RESULT_BINDING_ON_VOID,
                              "Result binding '" +
                                  spec.postResultBinding->name +
                                  "' cannot be used on a void function",
                              core::SourceLocation::empty());
  }

  for (const auto &expr : spec.post) {
    auto type = getExprType(expr);
    if (type.has_value() && type.value() != boolType) {
      reportTypeMismatch(boolType, type.value(), expr->location);
    }
  }
}

void TypeChecker::checkFunction(const Function &function) {
  checkFunctionSpecs(function.specifications, function.returnsVoid());

  // Set the current return type so return statements can be checked against it.
  if (!function.returnsVoid() &&
      function.returnType->identifier.symbolId.has_value()) {
    m_currentReturnType = function.returnType->identifier.symbolId;
  } else {
    // Implicit void, explicit void, or unresolved return type — default to
    // void.
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
                                "Return statement outside of function",
                                retStmt.location);
      return;
    }

    if (!retStmt.expression.has_value()) {
      // `return;` with no expression — only valid if the function returns void.
      if (!isVoid(*m_currentReturnType)) {
        // Expected a return value but got none.  Report using void as the
        // "actual" type so the message is meaningful.
        const auto *voidEntry = m_builtins.findType("void");
        if (voidEntry) {
          reportTypeMismatch(*m_currentReturnType, voidEntry->symbolId,
                             retStmt.location);
        }
      }
      return;
    }

    // Recursively check the return expression for sub-expression errors.
    checkExpression(retStmt.expression.value());

    // Expression is present — its type must match the function's return type.
    auto type = getExprType(retStmt.expression.value());
    if (type.has_value() && !expectType(*m_currentReturnType, type.value())) {
      reportTypeMismatch(*m_currentReturnType, type.value(), retStmt.location);
    }
  } else if (std::holds_alternative<IfStmt>(stmt)) {
    const IfStmt &statement = std::get<IfStmt>(stmt);
    // Recursively check the condition for sub-expression errors.
    checkExpression(statement.condition);

    // Check if the condition is a boolean expression
    auto conditionType = getExprType(statement.condition);
    if (!conditionType.has_value() || !isBoolean(*conditionType)) {
      m_diagnostics.reportError(core::ERROR_TYPE_MISMATCH,
                                "Condition must be a boolean expression",
                                statement.location);
      m_error = true;
    }

    checkStatement(*statement.consequent);
    if (statement.alternative) {
      // Optional<Ptr>
      checkStatement(**statement.alternative);
    }
  } else if (std::holds_alternative<WhileStmt>(stmt)) {
    const WhileStmt &statement = std::get<WhileStmt>(stmt);
    // Recursively check the condition for sub-expression errors.
    checkExpression(statement.condition);

    // Check if the condition is a boolean expression
    auto conditionType = getExprType(statement.condition);

    if (!conditionType.has_value() || !isBoolean(*conditionType)) {
      m_diagnostics.reportError(core::ERROR_TYPE_MISMATCH,
                                "Condition must be a boolean expression",
                                statement.location);
      m_error = true;
    }
    checkStatement(*statement.body);
  } else if (std::holds_alternative<ExprPtr>(stmt)) {
    const ExprPtr &expr = std::get<ExprPtr>(stmt);
    checkExpression(expr);
  } else if (std::holds_alternative<BlockPtr>(stmt)) {
    const BlockPtr &block = std::get<BlockPtr>(stmt);
    checkBlock(*block);
  } else if (std::holds_alternative<AssignmentStmt>(stmt)) {
    checkAssignment(std::get<AssignmentStmt>(stmt));
  }
}

void TypeChecker::checkDeclaration(const DeclStmt &decl) {
  checkExpression(decl.assignedExpression);

  // Check if assigned expression matches the declared type
  auto declType = decl.type.identifier.symbolId;
  auto exprType = getExprType(decl.assignedExpression);
  if (declType && exprType && !expectType(*declType, *exprType)) {
    reportTypeMismatch(*declType, *exprType, decl.location);
  }
}

void TypeChecker::checkAssignment(const AssignmentStmt &assignment) {
  checkExpression(assignment.value);

  // The identifier must have been resolved by the Resolver.
  if (!assignment.identifier.symbolId.has_value()) {
    // Unresolved symbol — the Resolver already reported this error.
    return;
  }

  const auto &symbol = m_table.get(*assignment.identifier.symbolId);

  // Check constness: only variables (not parameters) declared with `var` may be
  // assigned to.
  if (const auto *varInfo = std::get_if<VariableInfo>(&symbol.info)) {
    if (varInfo->isConstant) {
      m_error = true;
      m_diagnostics.reportError(core::ERROR_ASSIGN_TO_CONST,
                                "Cannot assign to constant variable '" +
                                    std::string(symbol.name.name()) + "'",
                                assignment.location);
      return;
    }

    // Check type compatibility between the variable and the assigned
    // expression.
    auto exprType = getExprType(assignment.value);
    if (exprType.has_value() && !expectType(varInfo->type, exprType.value())) {
      reportTypeMismatch(varInfo->type, exprType.value(), assignment.location);
    }
  } else if (std::holds_alternative<ParameterInfo>(symbol.info)) {
    // Parameters are immutable by default, and currently cannot be reassigned.
    // This will change in the future to allow mutable parameters.
    m_error = true;
    m_diagnostics.reportError(core::ERROR_ASSIGN_TO_CONST,
                              "Cannot assign to parameter '" +
                                  std::string(symbol.name.name()) + "'",
                              assignment.location);
  } else {
    // Assigning to a function, type, or other non-variable symbol.
    m_error = true;
    m_diagnostics.reportError(core::ERROR_ASSIGN_TO_CONST,
                              "Cannot assign to '" +
                                  std::string(symbol.name.name()) +
                                  "': not a variable",
                              assignment.location);
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
                                 "Tried to check a null expression",
                                 core::SourceLocation::empty());
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
      reportTypeMismatch(*leftType, *rightType, expr->location);
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
                                     const SymbolId &actual,
                                     const core::SourceLocation &location) {
  m_error = true;
  m_diagnostics.reportError(
      core::ERROR_TYPE_MISMATCH,
      "Type mismatch: expected '" + std::string(typeName(expected)) +
          "' but got '" + std::string(typeName(actual)) + "'",
      location);
}

std::string_view TypeChecker::typeName(const SymbolId &type) const {
  const auto &symbol = m_table.get(type);
  auto name = symbol.name.name();
  return name.empty() ? "<unknown>" : name;
}

bool TypeChecker::checkProgram(const std::shared_ptr<Root> &root) {
  if (!root) {
    return false;
  }

  for (auto &function : root->functions) {
    checkFunction(function);
  }
  return m_error;
}
} // namespace blaze::frontend
