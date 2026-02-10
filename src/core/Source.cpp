#include "core/Source.h"

#include <algorithm>
#include <fstream>

#include <tl/expected.hpp>

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
  return SourceLocation(SourceView::empty(), 0, 0);
}

SourceLocation::SourceLocation(const SourceView &view, size line, size column)
  : m_view(view), m_line(line), m_column(column) {}

const SourceView &SourceLocation::view() const { return m_view; }
std::string_view SourceLocation::text() const { return m_view.text(); }
size SourceLocation::offset() const { return m_view.offset(); }
size SourceLocation::length() const { return m_view.length(); }
size SourceLocation::line() const { return m_line; }
size SourceLocation::column() const { return m_column; }
Source::Source(std::string text, std::string path)
    : m_path(std::move(path)), m_text(std::move(text)) {}

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

} // namespace blaze::core
