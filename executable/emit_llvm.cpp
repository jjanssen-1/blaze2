#include <iostream>
#include <memory>
#include <string>

#include "backend/llvm/llvmbackend.h"
#include "core/Errors.h"
#include "frontend/Builtins.h"
#include "frontend/FrontendDriver.h"
#include "frontend/IrLower.h"
#include "frontend/SymbolTable.h"

static const char *severityToString(blaze::core::Severity severity) {
  switch (severity) {
  case blaze::core::Severity::info:
    return "info";
  case blaze::core::Severity::warning:
    return "warning";
  case blaze::core::Severity::error:
    return "error";
  case blaze::core::Severity::internal:
    return "internal";
  default:
    return "unknown";
  }
}

static void printDiagnostics(const blaze::core::DiagnosticList &diagnostics) {
  for (const auto &diag : diagnostics) {
    const auto &loc = diag.sourceLocation;
    if (loc.line() != 0) {
      std::cerr << loc.line() << ":" << loc.column() << ": ";
    }
    std::cerr << severityToString(diag.severity) << " [" << diag.errorCode
              << "]: " << diag.message << "\n";
  }
}

int main(int argc, char **argv) {
  if (argc < 3) {
    std::cerr << "Usage: emit_llvm <input-file> <output-ll>\n";
    return 1;
  }

  const std::string inputPath = argv[1];
  const std::string outputPath = argv[2];

  blaze::core::DiagnosticList diagnostics;

  // Parse, resolve, and type check.
  auto checked = blaze::frontend::checkFile(inputPath, diagnostics);
  if (!checked) {
    printDiagnostics(diagnostics);
    std::cerr << "Failed: " << blaze::frontend::toString(checked.error())
              << "\n";
    return 2;
  }

  // Lower to IR.
  auto symbolTable =
      std::make_shared<blaze::frontend::SymbolTable>(checked->resolve.symbols);
  auto builtins = std::make_shared<blaze::frontend::BuiltinRegistry>(
      checked->resolve.builtins);

  blaze::frontend::IRBuilder builder(diagnostics);
  auto functions = builder.lower(checked->parse.root, symbolTable, builtins);

  if (diagnostics.hasErrors()) {
    printDiagnostics(diagnostics);
    std::cerr << "IR lowering produced errors.\n";
    return 3;
  }

  // Generate LLVM IR.
  blaze::backend::LLVMBackend backend(diagnostics);
  auto status = backend.generate(functions);
  if (status != blaze::backend::CodeBackend::Success ||
      diagnostics.hasErrors()) {
    printDiagnostics(diagnostics);
    std::cerr << "LLVM code generation failed.\n";
    return 4;
  }

  // Emit LLVM IR to file.
  std::string error;
  if (!backend.emitIRToFile(outputPath, &error)) {
    std::cerr << "Failed to emit LLVM IR: " << error << "\n";
    return 5;
  }

  // Print any non-fatal diagnostics (warnings, info).
  printDiagnostics(diagnostics);
  return 0;
}
