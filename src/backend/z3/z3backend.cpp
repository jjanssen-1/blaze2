#include "z3backend.h"

#include <z3++.h>

blaze::backend::VcBackend::VerificationResult blaze::backend::Z3Backend::verify(
    const std::vector<frontend::IRFunction> &functions) {

  // Setup
  m_currentStatus = Status::Unknown;

  for (const auto &function : functions) {
    verifyFunction(function);
  }

  VerificationResult result;
  result.status = m_currentStatus;
  result.counterExamples = std::move(m_counterExamples);
  return result;
}
void blaze::backend::Z3Backend::verifyFunction(
    const frontend::IRFunction &function) {
  z3::solver solver(m_context);
}
