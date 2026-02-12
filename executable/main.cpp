#include <iostream>

#include "frontend/Builtins.h"
#include "frontend/FrontendDriver.h"
#include "frontend/IrLower.h"
#include "frontend/IrPrinter.h"

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
  if (argc < 2) {
    std::cerr << "Usage: blaze_cli <input-file>\n";
    return 1;
  }

  blaze::core::DiagnosticList diagnostics;

  // Parse, resolve, and type check.
  auto checked = blaze::frontend::checkFile(argv[1], diagnostics);
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

  // Print any non-fatal diagnostics (warnings, info).
  printDiagnostics(diagnostics);

  // Print IR to stdout.
  for (const auto &func : functions) {
    std::cout << blaze::frontend::irPrint(func);
  }

  return 0;
}
