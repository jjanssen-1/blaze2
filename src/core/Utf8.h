#pragma once

#include <cstddef>

namespace blaze::core {

// Returns the byte length of the UTF-8 codepoint starting at `lead`.
inline std::size_t utf8CodepointLength(unsigned char lead) {
  if ((lead & 0x80u) == 0x00u)
    return 1;
  if ((lead & 0xE0u) == 0xC0u)
    return 2;
  if ((lead & 0xF0u) == 0xE0u)
    return 3;
  if ((lead & 0xF8u) == 0xF0u)
    return 4;
  return 1;
}

} // namespace blaze::core
