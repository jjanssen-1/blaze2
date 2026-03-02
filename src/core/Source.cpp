#include "core/Source.h"
#include "core/Utf8.h"

#include <algorithm>
#include <fstream>

#include <tl/expected.hpp>

namespace {

using blaze::core::size;

} // namespace

namespace blaze::core {

const char *toString(SourceError error) {
  switch (error) {
  case SourceError::FailedToOpen:
    return "Failed to open file";
  default:
    return "Unknown source error";
  }
}

SourceView SourceView::empty() { return SourceView(); }

SourceView::SourceView(std::string_view text, size offset, size length)
    : text_(text), m_offset(offset), m_length(length) {}

std::string_view SourceView::text() const { return text_; }
size SourceView::offset() const { return m_offset; }
size SourceView::length() const { return m_length; }

SourceLocation SourceLocation::empty() {
  return SourceLocation(SourceView::empty(), 0, 0, 0, 0);
}

SourceLocation::SourceLocation(const SourceView &view, size line, size column,
                               size codepointOffset, size codepointLength)
    : m_view(view), m_line(line), m_column(column),
      m_codepointOffset(codepointOffset), m_codepointLength(codepointLength) {}

const SourceView &SourceLocation::view() const { return m_view; }
std::string_view SourceLocation::text() const { return m_view.text(); }
size SourceLocation::offset() const { return m_view.offset(); }
size SourceLocation::length() const { return m_view.length(); }
size SourceLocation::line() const { return m_line; }
size SourceLocation::column() const { return m_column; }
size SourceLocation::codepointOffset() const { return m_codepointOffset; }
size SourceLocation::codepointLength() const { return m_codepointLength; }

Source::Source(std::string text, std::string path)
    : m_path(std::move(path)), m_text(std::move(text)) {
  m_codepointOffsets.clear();
  m_codepointOffsets.push_back(0);
  for (size i = 0; i < m_text.size();) {
    const auto lead = static_cast<unsigned char>(m_text[i]);
    size advance = static_cast<size>(utf8CodepointLength(lead));
    if (i + advance > m_text.size())
      advance = 1;
    i += advance;
    m_codepointOffsets.push_back(i);
  }
}

tl::expected<Source, SourceError> Source::fromFile(const std::string &path) {
  std::ifstream stream(path, std::ios::in | std::ios::binary);
  if (!stream) {
    return tl::make_unexpected(SourceError::FailedToOpen);
  }

  std::string contents;
  stream.seekg(0, std::ios::end);
  contents.resize(static_cast<size>(stream.tellg()));
  stream.seekg(0, std::ios::beg);
  stream.read(contents.data(), static_cast<std::streamsize>(contents.size()));

  return Source(std::move(contents), path);
}

std::string_view Source::text() const { return m_text; }
const std::string &Source::path() const { return m_path; }
size Source::length() const { return m_text.size(); }

SourceView Source::view(size offset, size length) const {
  if (offset > m_text.size()) {
    return SourceView::empty();
  }
  const size safeLength = std::min(length, m_text.size() - offset);
  return SourceView(std::string_view(m_text).substr(offset, safeLength), offset,
                    safeLength);
}

size Source::byteOffsetForCodepoint(size codepointIndex) const {
  if (m_codepointOffsets.empty())
    return 0;
  if (codepointIndex >= m_codepointOffsets.size())
    return m_text.size();
  return m_codepointOffsets[codepointIndex];
}

size Source::byteLengthForCodepointRange(size startCodepoint,
                                         size endCodepointInclusive) const {
  if (m_codepointOffsets.empty())
    return 0;
  if (startCodepoint >= m_codepointOffsets.size())
    return 0;
  size endIndex = endCodepointInclusive + 1;
  if (endIndex >= m_codepointOffsets.size())
    endIndex = static_cast<size>(m_codepointOffsets.size() - 1);
  if (endIndex < startCodepoint)
    return 0;
  return m_codepointOffsets[endIndex] - m_codepointOffsets[startCodepoint];
}

SourceLocation Source::locationFromCodepointRange(size startCodepoint,
                                                  size endCodepointInclusive,
                                                  size line,
                                                  size column) const {
  size cpLength = 0;
  if (endCodepointInclusive >= startCodepoint) {
    cpLength = endCodepointInclusive - startCodepoint + 1;
  }
  const size byteOffset = byteOffsetForCodepoint(startCodepoint);
  const size byteLength =
      byteLengthForCodepointRange(startCodepoint, endCodepointInclusive);
  const auto view = Source::view(byteOffset, byteLength);
  return SourceLocation(view, line, column, startCodepoint, cpLength);
}

} // namespace blaze::core
