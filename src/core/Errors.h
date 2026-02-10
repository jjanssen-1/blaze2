#pragma once

#include "core/Common.h"
#include "core/Source.h"

#include <string>
#include <vector>

namespace blaze::core {

enum Severity { info, warning, error, internal };

struct Diagnostic {
  Severity severity;
  const u32 errorCode;
  const std::string message;
  const SourceLocation sourceLocation;
};

class DiagnosticList {
public:
  void reportError(const u32 &errorCode, const std::string &message,
                   const SourceLocation &location = SourceLocation::empty());

  void reportWarning(const u32 &errorCode, const std::string &message,
                     const SourceLocation &location = SourceLocation::empty());

  void reportInfo(const u32 &errorCode, const std::string &message,
                  const SourceLocation &location = SourceLocation::empty());

  void reportInternal(const u32 &errorCode, const std::string &message,
                      const SourceLocation &location = SourceLocation::empty());

  bool empty() const;
  size_t size() const;
  bool hasErrors() const;

  const std::vector<Diagnostic> &entries() const;
  std::vector<Diagnostic>::const_iterator begin() const;
  std::vector<Diagnostic>::const_iterator end() const;

private:
  std::vector<Diagnostic> m_diagnostics;
};

// Parsing errors
constexpr u32 ERROR_UNEXPECTED_TOKEN = 1000;
constexpr u32 ERROR_PARSE_EXCEPTION = 1001;
constexpr u32 ERROR_SOURCE_LOAD_FAILED = 1002;
constexpr u32 ERROR_ILLEGAL_STATEMENT = 1003;

// Semantic errors
constexpr u32 ERROR_DUPLICATE_SYMBOL = 2000;
constexpr u32 ERROR_UNRESOLVED_SYMBOL = 2001;
constexpr u32 ERROR_UNRESOLVED_TYPE = 2002;
constexpr u32 ERROR_UNRESOLVED_OVERLOAD = 2003;
constexpr u32 ERROR_AMBIGUOUS_OVERLOAD = 2004;
constexpr u32 ERROR_TYPE_MISMATCH = 2005;

// Internal errors
constexpr u32 ERROR_INTERNAL_ERROR = 9999;

} // namespace blaze::core
