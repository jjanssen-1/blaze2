#include "core/DiagnosticPrinter.h"

#include <fmt/format.h>
#include <sstream>
#include <vector>

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
  std::string sev_and_code = fmt::format("{}[{}]", sev, diag.errorCode);
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
    result += fmt::format("  --> {}:{}:{}\n", path, diag.sourceLocation.line(),
                          diag.sourceLocation.column());
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
  int errLine = diag.sourceLocation.line();
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

  endLine = std::min(endLine, (int)lines.size());
  startLine = std::min(startLine, (int)lines.size());

  std::string result;
  std::string gutterColor = m_options.useColor ? "\x1b[34m" : "";
  std::string resetColor = m_options.useColor ? "\x1b[0m" : "";

  result += fmt::format("{}   |{}\n", gutterColor, resetColor);

  for (int i = startLine; i <= endLine; ++i) {
    std::string lineText = lines[i - 1];
    result +=
        fmt::format("{}{:2} |{} {}\n", gutterColor, i, resetColor, lineText);

    if (i == errLine) {
      int col = diag.sourceLocation.column();
      int len = diag.sourceLocation.length();
      if (len <= 0)
        len = 1;
      std::string padding(std::max(0, col - 1), ' ');
      std::string carets(len, '^');
      std::string coloredCarets = colorize(diag.severity, carets);
      result += fmt::format("{}   |{} {}{}\n", gutterColor, resetColor, padding,
                            coloredCarets);
    }
  }

  result += fmt::format("{}   |{}\n", gutterColor, resetColor);

  return result;
}

} // namespace blaze::core
