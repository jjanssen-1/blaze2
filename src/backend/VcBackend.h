
#pragma once

#include "core/Source.h"
#include "frontend/Ir.h"

#include <vector>

namespace blaze::backend {

struct ModelValue {
  core::SourceLocation location;
  std::string value;
};

struct CounterExample {
  std::vector<ModelValue> values;
};

class VcBackend {
public:
  VcBackend() = default;
  enum Status { Verified, VerificationFailed, Disproven, Unknown, Error };

  struct VerificationResult {
    Status status;
    std::vector<CounterExample> counterExamples;
  };

  virtual VerificationResult
  verify(const std::vector<frontend::IRFunction> &functions) = 0;
};

} // namespace blaze::backend
