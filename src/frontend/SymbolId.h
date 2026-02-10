#pragma once

#include "core/Common.h"

#include <cstddef>
#include <functional>

namespace blaze::frontend {

class SymbolTable;

class SymbolId {
public:
  SymbolId() = delete;

  bool operator==(const SymbolId &other) const { return value == other.value; }
  bool operator!=(const SymbolId &other) const { return value != other.value; }

  core::u64 raw() const { return value; }

private:
  explicit SymbolId(core::u64 val) : value(val) {}

  core::u64 value;

  friend class SymbolTable;
};

struct SymbolIdHash {
  std::size_t operator()(const SymbolId &id) const noexcept {
    return std::hash<core::u64>{}(id.raw());
  }
};

} // namespace blaze::frontend
