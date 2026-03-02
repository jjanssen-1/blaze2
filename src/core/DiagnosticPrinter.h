// src/core/DiagnosticPrinter.h
#pragma once

#include "core/Errors.h"
#include "core/Source.h"

#include <string>

namespace blaze::core {

struct PrintOptions {
  bool useColor = true; // ANSI escape sequences
  int contextLines = 1; // lines above/below the error line to show
};

class DiagnosticPrinter {
public:
  explicit DiagnosticPrinter(const PrintOptions &options = {});

  // Format a single diagnostic into a printable string.
  std::string format(const Diagnostic &diag,
                     const Source *source = nullptr) const;

  // Convenience: format every diagnostic in the list.
  std::string format(const DiagnosticList &list,
                     const Source *source = nullptr) const;

private:
  PrintOptions m_options;

  std::string formatHeader(const Diagnostic &diag) const;
  std::string formatSnippet(const Diagnostic &diag, const Source &source) const;
  std::string severityLabel(Severity sev) const;
  std::string colorize(Severity sev, const std::string &text) const;
};

} // namespace blaze::core
