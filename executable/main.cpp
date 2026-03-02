#include <CLI/CLI.hpp>
#include <fmt/format.h>

#include "commands/BuildCommand.h"
#include "commands/EmitCommand.h"
#include "commands/RunCommand.h"
#include "commands/VerifyCommand.h"

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#include <unistd.h>
#endif

int main(int argc, char **argv) {
  CLI::App app{"blaze \xe2\x80\x94 the Blaze language compiler"};
  app.require_subcommand(1);

  bool isTTY = isatty(fileno(stderr));

  // Global flags
  bool noColor = !isTTY;
  bool verbose = false;
  app.add_flag("--no-color", noColor, "Disable colored output");
  app.add_flag("--verbose", verbose, "Enable verbose diagnostics");
  app.set_version_flag("--version", "blaze 0.1.0");

  // Subcommands
  blaze::cli::registerVerifyCommand(app, noColor, verbose);
  blaze::cli::registerBuildCommand(app, noColor, verbose);
  blaze::cli::registerEmitCommand(app, noColor, verbose);
  blaze::cli::registerRunCommand(app, noColor, verbose);

  CLI11_PARSE(app, argc, argv);
  return 0;
}
