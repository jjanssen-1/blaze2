#pragma once

#include "Ast.h"
#include "core/Errors.h"
#include "frontend/Builtins.h"
#include "frontend/SymbolTable.h"

#include <memory>

namespace blaze::frontend {

class TypeChecker {
public:
  explicit TypeChecker(const SymbolTable &table,
                       const BuiltinRegistry &builtins,
                       core::DiagnosticList &diagnostics)
      : m_table(table), m_builtins(builtins), m_diagnostics(diagnostics) {}

  bool checkProgram(const std::shared_ptr<Root> &root) {
    if (!root) {
      return false;
    }

    for (auto &function : root->functions) {
      checkFunction(function);
    }
    return m_error;
  }

private:
  std::optional<SymbolId> m_currentReturnType;
  const SymbolTable &m_table;
  const BuiltinRegistry &m_builtins;
  core::DiagnosticList &m_diagnostics;
  bool m_error = false;

  bool expectType(const SymbolId &expected, const SymbolId &actual);
  void checkFunction(const Function &function);
  void checkBlock(const Block &block);
  void checkStatement(const Statement &stmt);
  void checkDeclaration(const DeclStmt &decl);
  void checkExpression(const ExprPtr &expr);
  void checkFunctionSpecs(const FunctionSpecifications &spec);
  void reportTypeMismatch(const SymbolId &expected, const SymbolId &actual,
                          const core::SourceLocation &location);
  bool isVoid(const SymbolId &type);
  bool isBoolean(const SymbolId &type);
  std::string_view typeName(const SymbolId &type) const;
};

} // namespace blaze::frontend
