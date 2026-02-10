#pragma once

#include "frontend/Ast.h"
#include "frontend/SymbolId.h"

#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace blaze::frontend {

enum class SymbolKind { Variable, Function, Parameter, Type };

struct VariableInfo {
  SymbolId type;
  bool isConstant;
};

struct ParameterInfo {
  SymbolId type;
};

struct FunctionInfo {
  SymbolId returnType;
  std::vector<SymbolId> parameterTypes;
};

struct TypeInfo {
  std::optional<SymbolId> underlyingType;
};

using SymbolInfo =
    std::variant<VariableInfo, ParameterInfo, FunctionInfo, TypeInfo>;

class SymbolName {
public:
  SymbolName() = default;
  explicit SymbolName(const Identifier &identifier)
      : m_identifier(identifier) {}
  explicit SymbolName(std::string name) : m_builtinName(std::move(name)) {}

  bool isBuiltin() const { return !m_builtinName.empty(); }
  std::string_view name() const {
    if (isBuiltin()) {
      return m_builtinName;
    }
    return m_identifier ? std::string_view(m_identifier->name)
                        : std::string_view{};
  }
  const Identifier *identifier() const {
    return m_identifier ? &m_identifier.value() : nullptr;
  }

private:
  std::optional<Identifier> m_identifier;
  std::string m_builtinName;
};

struct Symbol {
  SymbolId id;
  SymbolKind kind;
  SymbolName name;
  std::optional<core::SourceLocation> location;
  SymbolInfo info;
};

} // namespace blaze::frontend
