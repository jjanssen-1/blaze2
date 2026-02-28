#pragma once

#include <string>
#include <string_view>
#include <vector>

#include <tl/expected.hpp>

#include "core/Common.h"

namespace blaze::core {

enum class SourceError { FailedToOpen };

const char *toString(SourceError error);

class SourceView {
public:
  static SourceView empty();

  std::string_view text() const;
  size offset() const;
  size length() const;

private:
  SourceView() = default;
  SourceView(std::string_view text, size offset, size length);

  friend class Source;

  std::string_view text_{};
  size m_offset = 0;
  size m_length = 0;
};

class SourceLocation {
public:
  static SourceLocation empty();

  SourceLocation(const SourceView &view, size line, size column,
                 size codepointOffset, size codepointLength);

  const SourceView &view() const;
  std::string_view text() const;
  size offset() const;
  size length() const;
  size line() const;
  size column() const;
  size codepointOffset() const;
  size codepointLength() const;

private:
  SourceView m_view;
  size m_line = 0;
  size m_column = 0;
  size m_codepointOffset = 0;
  size m_codepointLength = 0;
};

class Source {
public:
  Source() = default;
  explicit Source(std::string text, std::string path = {});

  static tl::expected<Source, SourceError> fromFile(const std::string &path);

  std::string_view text() const;
  const std::string &path() const;
  size length() const;

  SourceView view(size offset, size length) const;

  size byteOffsetForCodepoint(size codepointIndex) const;
  size byteLengthForCodepointRange(size startCodepoint,
                                   size endCodepointInclusive) const;
  SourceLocation locationFromCodepointRange(size startCodepoint,
                                            size endCodepointInclusive,
                                            size line, size column) const;

private:
  std::string m_path{};
  std::string m_text{};
  std::vector<size> m_codepointOffsets{};
};

} // namespace blaze::core
