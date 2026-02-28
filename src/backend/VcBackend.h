
#pragma once

#include "core/Source.h"
#include "frontend/Ir.h"

#include <vector>

namespace blaze::backend {

struct ParameterValue {
  core::SourceLocation location;
  std::string name;
  std::string value;
};

struct IntermediateValue {
  core::SourceLocation location;
  std::string value;
};

struct ViolatedPrecondition {
  std::string functionName;
  core::SourceLocation preconditionLocation;
};

struct CounterExample {
  core::SourceLocation checkLocation;
  std::vector<ParameterValue> parameters;
  std::vector<IntermediateValue> intermediates;
  std::optional<ViolatedPrecondition> violatedPrecondition;
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
