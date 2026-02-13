#pragma once

#include "backend/VcBackend.h"

#include <z3++.h>

namespace blaze::backend {

class Z3Backend : public VcBackend {
public:
  Z3Backend() = default;
  VerificationResult
  verify(const std::vector<frontend::IRFunction> &functions) override;

private:
  Status m_currentStatus;
  std::vector<CounterExample> m_counterExamples;
  z3::context m_context;

  void verifyFunction(const frontend::IRFunction &function);
};

} // namespace blaze::backend
