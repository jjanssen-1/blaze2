#pragma once

#include "core/Errors.h"
#include "frontend/Ast.h"
#include "frontend/Builtins.h"
#include "frontend/SymbolTable.h"

#include <memory>
#include <string>
#include <unordered_map>

#include <tl/expected.hpp>

namespace blaze::frontend {

enum class ResolveError { NullAst, InternalError };

const char *toString(ResolveError error);

struct ResolveResult {
  SymbolTable symbols;
  BuiltinRegistry builtins;
};

class Resolver {
public:
  explicit Resolver(core::DiagnosticList &diagnostics);

  tl::expected<ResolveResult, ResolveError>
  resolve(const std::shared_ptr<Root> &root);

  const BuiltinRegistry &builtins() const { return m_builtins; }

private:
  void resolveFunction(const Function &function);
  void resolveBlock(const Block &block);
  void resolveStatement(const Statement &statement);
  void resolveExpr(const ExprPtr &expr);
  void resolveFunctionSpecs(const FunctionSpecifications &spec);
  std::optional<SymbolId> resolveCallExpr(const CallExpr &call);
  std::optional<SymbolId> resolveExprType(const ExprPtr &expr);
  void pushScope();
  void popScope();
  bool hasScopedSymbol(const Identifier &identifier) const;
  void setScopedSymbol(const Identifier &identifier, SymbolId id);
  std::optional<SymbolId> findScopedSymbol(const Identifier &identifier) const;

  std::optional<SymbolId>
  resolveTypeSymbol(const TypeName &type, const core::SourceLocation &location);
  std::optional<SymbolId>
  resolveBuiltinType(std::string_view name,
                     const core::SourceLocation &location);
  void reportDuplicate(SymbolKind kind, const Identifier &identifier,
                       const core::SourceLocation &location);
  void reportUnresolved(const Identifier &identifier,
                        const core::SourceLocation &location);
  void reportUnresolvedType(std::string_view name,
                            const core::SourceLocation &location);
  void reportUnresolvedOverload(const Identifier &identifier,
                                const core::SourceLocation &location);
  void reportAmbiguousOverload(const Identifier &identifier,
                               const core::SourceLocation &location);

  core::DiagnosticList &m_diagnostics;
  SymbolTable m_symbols;
  BuiltinRegistry m_builtins;
  std::vector<std::unordered_map<std::string, SymbolId>> m_scopes;
};

} // namespace blaze::frontend
