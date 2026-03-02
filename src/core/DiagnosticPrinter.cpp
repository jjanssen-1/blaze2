#include "core/DiagnosticPrinter.h"
#include "core/Utf8.h"

#include <fmt/format.h>
#include <sstream>
#include <vector>

namespace {

using blaze::core::utf8CodepointLength;

struct DisplaySpan {
  std::size_t start = 0;
  std::size_t length = 0;
};

DisplaySpan computeDisplaySpan(const std::string &lineText,
                               std::size_t columnCp, std::size_t lengthCp,
                               std::size_t tabWidth) {
  DisplaySpan span{};
  std::size_t cpIndex = 0;
  std::size_t displayCol = 0;
  bool startSet = false;

  for (std::size_t i = 0; i < lineText.size();) {
    unsigned char lead = static_cast<unsigned char>(lineText[i]);
    std::size_t advance = utf8CodepointLength(lead);
    if (i + advance > lineText.size())
      advance = 1;

    const bool isTab = (advance == 1 && lineText[i] == '\t');
    const std::size_t tabAdvance =
        tabWidth == 0 ? 1 : (tabWidth - (displayCol % tabWidth));

    if (!startSet && cpIndex == columnCp) {
      span.start = displayCol;
      startSet = true;
    }

    if (cpIndex >= columnCp && cpIndex < columnCp + lengthCp) {
      if (isTab) {
        span.length += tabAdvance;
        displayCol += tabAdvance;
      } else {
        span.length += 1;
        displayCol += 1;
      }
    } else {
      if (isTab) {
        displayCol += tabAdvance;
      } else {
        displayCol += 1;
      }
    }

    cpIndex++;
    i += advance;
  }

  if (!startSet) {
    span.start = displayCol;
  }

  return span;
}

} // namespace

namespace blaze::core {

DiagnosticPrinter::DiagnosticPrinter(const PrintOptions &options)
    : m_options(options) {}

std::string DiagnosticPrinter::severityLabel(Severity sev) const {
  switch (sev) {
  case info:
    return "info";
  case warning:
    return "warning";
  case error:
    return "error";
  case internal:
    return "internal";
  default:
    return "unknown";
  }
}

std::string DiagnosticPrinter::colorize(Severity sev,
                                        const std::string &text) const {
  if (!m_options.useColor) {
    return text;
  }
  switch (sev) {
  case info:
    return fmt::format("\x1b[36m{}\x1b[0m", text);
  case warning:
    return fmt::format("\x1b[33;1m{}\x1b[0m", text);
  case error:
    return fmt::format("\x1b[31;1m{}\x1b[0m", text);
  case internal:
    return fmt::format("\x1b[35;1m{}\x1b[0m", text);
  default:
    return text;
  }
}

std::string DiagnosticPrinter::formatHeader(const Diagnostic &diag) const {
  std::string sev = severityLabel(diag.severity);
  std::string sev_and_code = fmt::format("{}[BLZ{}]", sev, diag.errorCode);
  if (m_options.useColor) {
    sev_and_code = colorize(diag.severity, sev_and_code);
  }
  return fmt::format("{}: {}", sev_and_code, diag.message);
}

std::string DiagnosticPrinter::format(const Diagnostic &diag,
                                      const Source *source) const {
  std::string result = formatHeader(diag) + "\n";
  if (diag.sourceLocation.line() > 0) {
    std::string path = source ? source->path() : "unknown";
    // Column is stored as 0-based (from ANTLR); display as 1-based.
    size_t displayCol = diag.sourceLocation.column() + 1;
    result += fmt::format("  --> {}:{}:{}\n", path, diag.sourceLocation.line(),
                          displayCol);
    if (source) {
      result += formatSnippet(diag, *source);
    }
  }
  return result;
}

std::string DiagnosticPrinter::format(const DiagnosticList &list,
                                      const Source *source) const {
  std::string result;
  for (const auto &diag : list) {
    result += format(diag, source);
  }
  return result;
}

std::string DiagnosticPrinter::formatSnippet(const Diagnostic &diag,
                                             const Source &source) const {
  int errLine = static_cast<int>(diag.sourceLocation.line());
  int startLine = std::max(1, errLine - m_options.contextLines);
  int endLine = errLine + m_options.contextLines;

  std::string_view textView = source.text();
  std::string text(textView.data(), textView.size());
  std::vector<std::string> lines;
  std::istringstream iss(text);
  std::string line;
  while (std::getline(iss, line)) {
    if (!line.empty() && line.back() == '\r') {
      line.pop_back();
    }
    lines.push_back(line);
  }

  if (lines.empty())
    return "";

  endLine = std::min(endLine, static_cast<int>(lines.size()));
  startLine = std::min(startLine, static_cast<int>(lines.size()));

  // Compute the gutter width dynamically so it accommodates the largest
  // line number we'll print.  A line number N needs floor(log10(N))+1 digits.
  // We always use at least 2 columns so short files look clean.
  int gutterWidth = 2;
  {
    int maxLine = endLine;
    int w = 0;
    int n = maxLine;
    do {
      ++w;
      n /= 10;
    } while (n > 0);
    if (w > gutterWidth)
      gutterWidth = w;
  }

  // Helper: blank gutter line  "   |"  (gutterWidth spaces + " |")
  // Helper: numbered gutter    " 3 |"  (right-aligned number + " |")
  // After the pipe we always emit a single space before the source text,
  // so the total prefix length is (gutterWidth + 3): spaces/digits + " | ".

  std::string result;
  std::string gutterColor = m_options.useColor ? "\x1b[34m" : "";
  std::string resetColor = m_options.useColor ? "\x1b[0m" : "";

  // Blank separator before the snippet.
  result += fmt::format("{}{} |{}\n", gutterColor,
                        std::string(gutterWidth, ' '), resetColor);

  for (int i = startLine; i <= endLine; ++i) {
    const std::string &lineText = lines[static_cast<size_t>(i - 1)];

    // Right-align the line number within gutterWidth columns.
    result += fmt::format("{}{:>{}} |{} {}\n", gutterColor, i, gutterWidth,
                          resetColor, lineText);

    if (i == errLine) {
      // Column is 0-based (ANTLR charPositionInLine). It counts codepoints,
      // not display columns. We compute a display-aware span to handle tabs
      // and UTF-8 safely.
      size_t col = diag.sourceLocation.column();
      size_t cpLen = diag.sourceLocation.codepointLength();

      size_t tabWidth = 1;
      if (m_options.tabWidth > 0) {
        tabWidth = static_cast<size_t>(m_options.tabWidth);
      }
      auto span = computeDisplaySpan(lineText, col, cpLen, tabWidth);
      size_t paddingWidth = span.start;
      size_t caretWidth = span.length;
      if (caretWidth == 0)
        caretWidth = 1;

      std::string padding(paddingWidth, ' ');
      std::string carets(caretWidth, '^');
      std::string coloredCarets = colorize(diag.severity, carets);

      // The blank gutter for caret lines uses the same width as numbered ones.
      result += fmt::format("{}{} |{} {}{}\n", gutterColor,
                            std::string(gutterWidth, ' '), resetColor, padding,
                            coloredCarets);
    }
  }

  // Blank separator after the snippet.
  result += fmt::format("{}{} |{}\n", gutterColor,
                        std::string(gutterWidth, ' '), resetColor);

  return result;
}

} // namespace blaze::core
