#include "Builtins.h"

#include "frontend/Symbol.h"

namespace blaze::frontend {

namespace {
struct BuiltinSpec {
  std::string_view name;
  bool isIntegral;
  bool supportsArithmetic;
};

constexpr BuiltinSpec kBuiltinSpecs[] = {
    {"void", false, false},
    {"i32", true, true},
    {"bool", false, false},
};

} // namespace

const BuiltinTypeEntry *BuiltinRegistry::findType(std::string_view name) const {
  for (const auto &entry : m_types) {
    if (entry.info.name == name) {
      return &entry;
    }
  }
  return nullptr;
}

BuiltinRegistry registerBuiltinSymbols(SymbolTable &symbols) {
  BuiltinRegistry registry;
  registry.m_types.reserve(std::size(kBuiltinSpecs));

  for (std::size_t index = 0; index < std::size(kBuiltinSpecs); ++index) {
    const auto &spec = kBuiltinSpecs[index];
    TypeInfo typeInfo{};
    SymbolId symbolId =
        symbols.addBuiltinSymbol(SymbolKind::Type, spec.name, typeInfo);

    BuiltinTypeEntry entry{
        BuiltinTypeInfo{std::string(spec.name), spec.isIntegral,
                        spec.supportsArithmetic},
        symbolId};
    registry.m_types.push_back(std::move(entry));
  }

  return registry;
}

} // namespace blaze::frontend
