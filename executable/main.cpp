#include <iostream>

#include "frontend/FrontendDriver.h"

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << "Usage: blaze_cli <input-file>\n";
    return 1;
  }

  blaze::core::DiagnosticList diagnostics;
  auto result = blaze::frontend::parseFile(argv[1], diagnostics);
  if (!result) {
    std::cerr << "Parse failed: " << blaze::frontend::toString(result.error())
              << "\n";
    return 2;
  }
  if (!result.value().root) {
    std::cerr << "Parse failed with " << diagnostics.size()
              << " diagnostic(s)." << "\n";
    return 2;
  }

  std::cout << "Parse ok\n";
  return 0;
}
