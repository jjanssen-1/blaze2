#pragma once

#include "frontend/Ir.h"

#include <vector>

namespace blaze::backend {

class CodeBackend {
public:
  CodeBackend() = default;
  virtual ~CodeBackend() = default;

  enum Status { Success, Error };

  virtual Status
  generate(const std::vector<frontend::IRFunction> &functions) = 0;
};

} // namespace blaze::backend
