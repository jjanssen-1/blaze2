// src/driver/Driver.h
#pragma once

#include "core/Errors.h"
#include "driver/CompileOptions.h"

#include <optional>
#include <string>

namespace blaze::driver {

struct DriverResult {
  enum Status { Ok, CompileError, VerificationFailed, InternalError };
  Status status = Status::Ok;
  core::DiagnosticList diagnostics;
  std::optional<std::string> irText;
};

// Run the full pipeline described by `options`.
// This is the single entry point that every CLI command calls.
DriverResult run(const CompileOptions &options);

} // namespace blaze::driver
