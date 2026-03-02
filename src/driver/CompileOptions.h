// src/driver/CompileOptions.h
#pragma once

#include <string>
#include <vector>

namespace blaze::driver {

enum class OutputKind {
  PrintIR,        // Dump textual IR to stdout
  EmitLLVM,       // Write .ll file
  EmitObject,     // Write .o file
  EmitExecutable, // Write a linked binary
};

struct CompileOptions {
  std::string inputFile;

  // What to produce — irrelevant when only verifying.
  OutputKind outputKind = OutputKind::EmitExecutable;
  std::string outputFile; // empty → derive from inputFile

  // Verification
  bool verify = false;     // run Z3 verification pass
  bool verifyOnly = false; // if true, skip codegen entirely

  // Linking (only used for EmitExecutable)
  std::string linkerPath; // empty → auto-detect
  std::vector<std::string> linkerArgs;

  // Behaviour knobs
  bool printIR = false; // additionally dump IR (even when compiling)
  bool noColor = false; // disable ANSI colors
  bool verbose = false; // extra diagnostic detail
};

} // namespace blaze::driver
