#include "Resolver.h"
#include "Ast.h"

#include <string>
#include <variant>

namespace blaze::frontend {

const char *toString(ResolveError error) {
  switch (error) {
  case ResolveError::NullAst:
    return "AST root is null";
  case ResolveError::InternalError:
    return "Resolver internal error";
  default:
    return "Unknown resolve error";
  }
}

Resolver::Resolver(core::DiagnosticList &diagnostics)
    : m_diagnostics(diagnostics), m_symbols(),
      m_builtins(registerBuiltinSymbols(m_symbols)) {}

tl::expected<ResolveResult, ResolveError>
Resolver::resolve(const std::shared_ptr<Root> &root) {
  if (!root) {
    return tl::make_unexpected(ResolveError::NullAst);
  }

  for (const auto &function : root->functions) {
    resolveFunction(function);
  }

  return ResolveResult{std::move(m_symbols), m_builtins};
}

void Resolver::resolveFunction(const Function &function) {
  pushScope();
  std::vector<SymbolId> parameterTypeIds;
  parameterTypeIds.reserve(function.parameters.size());
  bool hasTypeErrors = false;
  std::optional<SymbolId> returnTypeId;
  if (function.returnType.has_value()) {
    returnTypeId = resolveTypeSymbol(*function.returnType, function.location);
    if (returnTypeId.has_value()) {
      function.returnType->identifier.symbolId = returnTypeId.value();
    }
  } else {
    // Default return type.
    returnTypeId = resolveBuiltinType("void", function.location);
  }
  if (!returnTypeId.has_value()) {
    hasTypeErrors = true;
  }

  // Resolve function specifications
  resolveFunctionSpecs(function.specifications);

  // Resolve parameters
  for (const auto &param : function.parameters) {
    const auto typeId = resolveTypeSymbol(param.type, function.location);
    if (!typeId.has_value()) {
      hasTypeErrors = true;
    } else {
      param.type.identifier.symbolId = typeId.value();
      parameterTypeIds.push_back(typeId.value());
    }

    if (hasScopedSymbol(param.identifier)) {
      reportDuplicate(SymbolKind::Parameter, param.identifier,
                      function.location);
    } else if (typeId.has_value()) {
      ParameterInfo info{typeId.value()};
      const auto paramId = m_symbols.addSymbol(
          SymbolKind::Parameter, param.identifier, function.location, info);
      param.identifier.symbolId = paramId;
      setScopedSymbol(param.identifier, paramId);
    }
  }

  // Only resolve function if there were no errors when resolving the
  // return/argument types
  if (!hasTypeErrors && returnTypeId.has_value()) {
    bool isDuplicate = false;
    const auto existingFunctions =
        m_symbols.findAll(SymbolKind::Function, function.identifier);
    for (const auto &symbolId : existingFunctions) {
      const auto &symbol = m_symbols.get(symbolId);
      if (const auto *info = std::get_if<FunctionInfo>(&symbol.info)) {
        if (info->parameterTypes == parameterTypeIds) {
          isDuplicate = true;
          break;
        }
      }
    }

    if (isDuplicate) {
      reportDuplicate(SymbolKind::Function, function.identifier,
                      function.location);
    } else {
      FunctionInfo info{returnTypeId.value(), parameterTypeIds};
      const auto fnId = m_symbols.addSymbol(
          SymbolKind::Function, function.identifier, function.location, info);
      function.identifier.symbolId = fnId;
    }
  }

  resolveBlock(function.body);
  popScope();
}

void Resolver::resolveBlock(const Block &block) {
  for (const auto &statement : block.statements) {
    resolveStatement(statement);
  }
}

void Resolver::resolveStatement(const Statement &statement) {
  // Currently resolve most of the statements in this function
  if (std::holds_alternative<DeclStmt>(statement)) {
    const auto &decl = std::get<DeclStmt>(statement);
    auto typeId = resolveTypeSymbol(decl.type, decl.location);

    if (typeId.has_value()) {
      decl.type.identifier.symbolId = typeId.value();
    }

    if (hasScopedSymbol(decl.identifier)) {
      reportDuplicate(SymbolKind::Variable, decl.identifier, decl.location);
    } else if (typeId.has_value()) {
      VariableInfo info{typeId.value(), decl.isConstant};
      const auto varId = m_symbols.addSymbol(
          SymbolKind::Variable, decl.identifier, decl.location, info);
      decl.identifier.symbolId = varId;

      setScopedSymbol(decl.identifier, varId);
    }
    resolveExpr(decl.assignedExpression);
  } else if (std::holds_alternative<ReturnStmt>(statement)) {
    const auto &retStmt = std::get<ReturnStmt>(statement);

    if (retStmt.expression.has_value()) {
      resolveExpr(retStmt.expression.value());
    }
  } else if (std::holds_alternative<IfStmt>(statement)) {
    const auto &ifStmt = std::get<IfStmt>(statement);
    resolveExpr(ifStmt.condition);
    if (ifStmt.consequent) {
      resolveStatement(*ifStmt.consequent);
    } // else: internal error

    if (ifStmt.alternative.has_value() && ifStmt.alternative.value()) {
      resolveStatement(*ifStmt.alternative.value());
    }
  } else if (std::holds_alternative<WhileStmt>(statement)) {
    const auto &whileStmt = std::get<WhileStmt>(statement);
    resolveExpr(whileStmt.condition);

    if (whileStmt.body) {
      resolveStatement(*whileStmt.body);
    }
  } else if (std::holds_alternative<ExprPtr>(statement)) {
    resolveExpr(std::get<ExprPtr>(statement));
  } else if (std::holds_alternative<BlockPtr>(statement)) {
    resolveBlock(*std::get<BlockPtr>(statement));
  } else if (std::holds_alternative<AssignmentStmt>(statement)) {
    const auto &assignmentStmt = std::get<AssignmentStmt>(statement);

    // Resolve the left-hand side identifier.
    if (const auto scopedId = findScopedSymbol(assignmentStmt.identifier)) {
      assignmentStmt.identifier.symbolId = scopedId.value();
    } else {
      reportUnresolved(assignmentStmt.identifier, assignmentStmt.location);
    }

    // Resolve the right-hand side expression.
    // Potentially the lhs will also need expression resolution if an index
    // operator gets added.
    resolveExpr(assignmentStmt.value);
  }
}

void Resolver::resolveExpr(const ExprPtr &expr) {
  if (!expr) {
    // Internal error
    return;
  }
  if (std::holds_alternative<BinaryExpr>(*expr)) {
    const auto &bin = std::get<BinaryExpr>(*expr);

    resolveExpr(bin.left);
    resolveExpr(bin.right);
  } else if (std::holds_alternative<CallExpr>(*expr)) {
    const auto &call = std::get<CallExpr>(*expr);

    for (const auto &arg : call.arguments) {
      resolveExpr(arg);
    }
    resolveCallExpr(call, expr->location);
  } else if (std::holds_alternative<VarExpr>(*expr)) {
    const auto &var = std::get<VarExpr>(*expr);

    if (const auto scopedId = findScopedSymbol(var.identifier)) {
      var.identifier.symbolId = scopedId.value();
    } else {
      reportUnresolved(var.identifier, expr->location);
    }
  } else if (std::holds_alternative<UnaryExpr>(*expr)) {
    const auto &unaryExpr = std::get<UnaryExpr>(*expr);
    resolveExpr(unaryExpr.operand);
  }

  // Store the resolved type on the expression node
  auto type = resolveExprType(expr);
  if (type.has_value()) {
    setExprType(expr, type.value());
  }
}

std::optional<SymbolId>
Resolver::resolveCallExpr(const CallExpr &call,
                          const core::SourceLocation &location) {
  // Find all functions with the given name
  const auto candidates =
      m_symbols.findAll(SymbolKind::Function, call.identifier);
  if (candidates.empty()) {
    reportUnresolvedOverload(call.identifier, location);
    return std::nullopt;
  }

  std::vector<std::optional<SymbolId>> argTypes;
  argTypes.reserve(call.arguments.size());
  for (const auto &arg : call.arguments) {
    argTypes.push_back(resolveExprType(arg));
  }

  SymbolId matchedId = candidates.front();
  bool hasMatch = false;

  // Check if one of the candidates matches the argument types
  for (const auto &symbolId : candidates) {
    const auto &symbol = m_symbols.get(symbolId);
    const auto *info = std::get_if<FunctionInfo>(&symbol.info);

    if (!info || info->parameterTypes.size() != argTypes.size()) {
      continue;
    }
    bool match = true;
    for (std::size_t i = 0; i < argTypes.size(); ++i) {
      if (!argTypes[i].has_value() ||
          info->parameterTypes[i] != argTypes[i].value()) {
        match = false;
        break;
      }
    }
    if (match) {
      if (hasMatch) {
        // Potentially redundant. Ambiguous overloads should be caught in
        // function declaration resolving.
        reportAmbiguousOverload(call.identifier, location);
        return std::nullopt;
      }
      matchedId = symbolId;
      hasMatch = true;
    }
  }

  if (!hasMatch) {
    reportUnresolvedOverload(call.identifier, location);
    return std::nullopt;
  }

  call.identifier.symbolId = matchedId;
  return matchedId;
}

std::optional<SymbolId> Resolver::resolveExprType(const ExprPtr &expr) {
  if (!expr) {
    return std::nullopt;
  }
  if (std::holds_alternative<IntExpr>(*expr)) {
    // Currently i32 by default, later a comptime_int could be added to allow
    // easy conversion for the int literal.
    if (const auto *builtin = m_builtins.findType("i32")) {
      return builtin->symbolId;
    }
    return std::nullopt;
  }
  if (std::holds_alternative<BoolExpr>(*expr)) {
    if (const auto *builtin = m_builtins.findType("bool")) {
      return builtin->symbolId;
    }
    return std::nullopt;
  }
  if (std::holds_alternative<VarExpr>(*expr)) {
    const auto &var = std::get<VarExpr>(*expr);
    if (const auto scopedId = findScopedSymbol(var.identifier)) {
      const auto &symbol = m_symbols.get(scopedId.value());
      if (const auto *info = std::get_if<VariableInfo>(&symbol.info)) {
        return info->type;
      }
      if (const auto *info = std::get_if<ParameterInfo>(&symbol.info)) {
        return info->type;
      }
    }
    return std::nullopt;
  }
  if (std::holds_alternative<CallExpr>(*expr)) {
    const auto &call = std::get<CallExpr>(*expr);
    auto resolvedId = resolveCallExpr(call, expr->location);
    if (resolvedId.has_value()) {
      const auto &symbol = m_symbols.get(resolvedId.value());
      if (const auto *info = std::get_if<FunctionInfo>(&symbol.info)) {
        return info->returnType;
      }
    }
    return std::nullopt;
  }
  // For now, the operators will simply return their first operands type.
  // Later this will be extended to support operator overloading.
  if (std::holds_alternative<UnaryExpr>(*expr)) {
    const auto &unaryExpr = std::get<UnaryExpr>(*expr);
    return resolveExprType(unaryExpr.operand);
  }
  if (std::holds_alternative<BinaryExpr>(*expr)) {
    const auto &binaryExpr = std::get<BinaryExpr>(*expr);
    return resolveExprType(binaryExpr.left);
  }

  return std::nullopt;
}

void Resolver::pushScope() { m_scopes.emplace_back(); }

void Resolver::popScope() {
  if (!m_scopes.empty()) {
    m_scopes.pop_back();
  }
}

bool Resolver::hasScopedSymbol(const Identifier &identifier) const {
  const auto name = identifier.name;
  for (auto it = m_scopes.rbegin(); it != m_scopes.rend(); ++it) {
    if (it->find(name) != it->end()) {
      return true;
    }
  }
  return false;
}

void Resolver::setScopedSymbol(const Identifier &identifier, SymbolId id) {
  if (m_scopes.empty()) {
    pushScope();
  }
  auto &scope = m_scopes.back();
  scope.insert_or_assign(identifier.name, id);
}

std::optional<SymbolId>
Resolver::findScopedSymbol(const Identifier &identifier) const {
  const auto name = identifier.name;
  for (auto it = m_scopes.rbegin(); it != m_scopes.rend(); ++it) {
    const auto found = it->find(name);
    if (found != it->end()) {
      return found->second;
    }
  }
  return std::nullopt;
}

std::optional<SymbolId>
Resolver::resolveTypeSymbol(const TypeName &type,
                            const core::SourceLocation &location) {
  if (const auto *builtin = m_builtins.findType(type.identifier.name)) {
    return builtin->symbolId;
  }

  auto existing = m_symbols.find(SymbolKind::Type, type.identifier);
  if (existing.has_value()) {
    return existing.value();
  }

  reportUnresolvedType(type.identifier.name, location);
  return std::nullopt;
}

std::optional<SymbolId>
Resolver::resolveBuiltinType(std::string_view name,
                             const core::SourceLocation &location) {
  if (const auto *builtin = m_builtins.findType(name)) {
    return builtin->symbolId;
  }

  reportUnresolvedType(name, location);
  return std::nullopt;
}

void Resolver::reportDuplicate(SymbolKind kind, const Identifier &identifier,
                               const core::SourceLocation &location) {
  const auto kindLabel = static_cast<core::u32>(kind);
  m_diagnostics.reportError(
      core::ERROR_DUPLICATE_SYMBOL,
      "Duplicate symbol (kind " + std::to_string(kindLabel) + ")", location);
}

void Resolver::reportUnresolved(const Identifier &identifier,
                                const core::SourceLocation &location) {
  m_diagnostics.reportError(core::ERROR_UNRESOLVED_SYMBOL,
                            "Unresolved symbol: " + identifier.name, location);
}

void Resolver::reportUnresolvedType(std::string_view name,
                                    const core::SourceLocation &location) {
  m_diagnostics.reportError(core::ERROR_UNRESOLVED_TYPE,
                            "Unresolved type: " + std::string(name), location);
}

void Resolver::reportUnresolvedOverload(const Identifier &identifier,
                                        const core::SourceLocation &location) {
  m_diagnostics.reportError(core::ERROR_UNRESOLVED_OVERLOAD,
                            "Unresolved overload: " + identifier.name,
                            location);
}

void Resolver::reportAmbiguousOverload(const Identifier &identifier,
                                       const core::SourceLocation &location) {
  m_diagnostics.reportError(core::ERROR_AMBIGUOUS_OVERLOAD,
                            "Ambiguous overload: " + identifier.name, location);
}

void Resolver::resolveFunctionSpecs(const FunctionSpecifications &spec) {
  for (const auto &expr : spec.pre) {
    resolveExpr(expr);
  }

  for (const auto &expr : spec.post) {
    resolveExpr(expr);
  }
}
} // namespace blaze::frontend
