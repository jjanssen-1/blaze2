#pragma once

#include "frontend/Symbol.h"

#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace blaze::frontend {

class SymbolTable {
public:
  SymbolTable() = default;

  SymbolId addSymbol(SymbolKind kind, const Identifier &name,
                     const core::SourceLocation &location, SymbolInfo info);
  SymbolId addBuiltinSymbol(SymbolKind kind, std::string_view name,
                            SymbolInfo info);

  const Symbol &get(SymbolId id) const;
  Symbol &get(SymbolId id);

  std::optional<SymbolId> find(SymbolKind kind, const Identifier &name) const;
  std::optional<SymbolId> find(SymbolKind kind, std::string_view name) const;
  std::vector<SymbolId> findAll(SymbolKind kind, const Identifier &name) const;
  std::vector<SymbolId> findAll(SymbolKind kind, std::string_view name) const;

  core::size count() const;

private:
  struct SymbolKey {
    SymbolKind kind;
    std::string name;

    bool operator==(const SymbolKey &other) const {
      return kind == other.kind && name == other.name;
    }
  };

  struct SymbolKeyHash {
    std::size_t operator()(const SymbolKey &key) const noexcept {
      std::size_t seed = 0;
      seed ^= std::hash<int>{}(static_cast<int>(key.kind)) + 0x9e3779b9 +
              (seed << 6) + (seed >> 2);
      seed ^= std::hash<std::string>{}(key.name) + 0x9e3779b9 + (seed << 6) +
              (seed >> 2);
      return seed;
    }
  };

  core::u64 nextId = 1;
  std::vector<Symbol> symbols;
  std::unordered_map<SymbolKey, std::vector<SymbolId>, SymbolKeyHash> index;
};

} // namespace blaze::frontend
