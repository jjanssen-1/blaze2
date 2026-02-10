#include "frontend/SymbolTable.h"

namespace blaze::frontend {

SymbolId SymbolTable::addSymbol(SymbolKind kind, const Identifier &name,
                                const core::SourceLocation &location,
                                SymbolInfo info) {
  const SymbolId id = SymbolId(nextId++);
  symbols.push_back(
      Symbol{id, kind, SymbolName{name}, location, std::move(info)});
  index[SymbolKey{kind, name.name}].push_back(id);
  return id;
}

SymbolId SymbolTable::addBuiltinSymbol(SymbolKind kind, std::string_view name,
                                       SymbolInfo info) {
  const SymbolId id = SymbolId(nextId++);
  symbols.push_back(Symbol{id, kind, SymbolName{std::string(name)}, std::nullopt,
                           std::move(info)});
  index[SymbolKey{kind, std::string(name)}].push_back(id);
  return id;
}

const Symbol &SymbolTable::get(SymbolId id) const {
  return symbols[id.raw() - 1];
}

Symbol &SymbolTable::get(SymbolId id) { return symbols[id.raw() - 1]; }

std::optional<SymbolId> SymbolTable::find(SymbolKind kind,
                                          const Identifier &name) const {
  auto values = findAll(kind, name);
  if (values.empty()) {
    return std::nullopt;
  }
  return values.front();
}

std::optional<SymbolId> SymbolTable::find(SymbolKind kind,
                                          std::string_view name) const {
  auto values = findAll(kind, name);
  if (values.empty()) {
    return std::nullopt;
  }
  return values.front();
}

std::vector<SymbolId> SymbolTable::findAll(SymbolKind kind,
                                           const Identifier &name) const {
  auto it = index.find(SymbolKey{kind, name.name});
  if (it == index.end()) {
    return {};
  }
  return it->second;
}

std::vector<SymbolId> SymbolTable::findAll(SymbolKind kind,
                                           std::string_view name) const {
  auto it = index.find(SymbolKey{kind, std::string(name)});
  if (it == index.end()) {
    return {};
  }
  return it->second;
}

core::size SymbolTable::count() const {
  return static_cast<core::size>(symbols.size());
}

} // namespace blaze::frontend
