#include "Errors.h"

namespace blaze::core {

void DiagnosticList::reportError(const u32 &errorCode,
                                 const std::string &message,
                                 const SourceLocation &location) {
  m_diagnostics.push_back({error, errorCode, message, location});
}

void DiagnosticList::reportWarning(const u32 &errorCode,
                                   const std::string &message,
                                   const SourceLocation &location) {
  m_diagnostics.push_back({warning, errorCode, message, location});
}

void DiagnosticList::reportInfo(const u32 &errorCode,
                                const std::string &message,
                                const SourceLocation &location) {
  m_diagnostics.push_back({info, errorCode, message, location});
}

void DiagnosticList::reportInternal(const u32 &errorCode,
                                    const std::string &message,
                                    const SourceLocation &location) {
  m_diagnostics.push_back({internal, errorCode, message, location});
}

bool DiagnosticList::empty() const { return m_diagnostics.empty(); }
size_t DiagnosticList::size() const { return m_diagnostics.size(); }
bool DiagnosticList::hasErrors() const {
  for (const auto &diag : m_diagnostics) {
    if (diag.severity == error || diag.severity == internal) {
      return true;
    }
  }
  return false;
}
const std::vector<Diagnostic> &DiagnosticList::entries() const {
  return m_diagnostics;
}
std::vector<Diagnostic>::const_iterator DiagnosticList::begin() const {
  return m_diagnostics.begin();
}
std::vector<Diagnostic>::const_iterator DiagnosticList::end() const {
  return m_diagnostics.end();
}
} // namespace blaze::core
