#pragma once

#include "frontend/SymbolTable.h"

#include <string>
#include <string_view>
#include <vector>

namespace blaze::frontend {

struct BuiltinTypeInfo {
  std::string name;
  bool isIntegral = false;
  bool supportsArithmetic = false;
};

struct BuiltinTypeEntry {
  BuiltinTypeInfo info;
  SymbolId symbolId;
};

class BuiltinRegistry {
public:
  const std::vector<BuiltinTypeEntry> &types() const { return m_types; }
  const BuiltinTypeEntry *findType(std::string_view name) const;

private:
  std::vector<BuiltinTypeEntry> m_types;

  friend BuiltinRegistry registerBuiltinSymbols(SymbolTable &symbols);
};

BuiltinRegistry registerBuiltinSymbols(SymbolTable &symbols);

} // namespace blaze::frontend
