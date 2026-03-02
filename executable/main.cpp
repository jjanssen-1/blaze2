#include <CLI/CLI.hpp>
#include <fmt/format.h>

#include <filesystem>

#include "commands/BuildCommand.h"
#include "commands/EmitCommand.h"
#include "commands/RunCommand.h"
#include "commands/VerifyCommand.h"
#include "core/DiagnosticPrinter.h"
#include "core/Source.h"
#include "driver/Driver.h"

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#include <unistd.h>
#endif

int main(int argc, char **argv) {
  CLI::App app{"blaze \xe2\x80\x94 the Blaze language compiler"};
  app.require_subcommand(0);
  app.allow_extras();

  bool isTTY = isatty(fileno(stderr));

  // Global flags
  bool noColor = !isTTY;
  bool verbose = false;
  app.add_flag("--no-color", noColor, "Disable colored output");
  app.add_flag("--verbose", verbose, "Enable verbose diagnostics");
  app.set_version_flag("--version", "blaze 0.1.0");

  // Default (no subcommand) flags
  bool emitLlvm = false;
  bool compileOnly = false;
  std::string outputFile;
  std::string linkerPath;
  std::vector<std::string> linkerArgs;

  app.add_flag("--emit-llvm", emitLlvm, "Write LLVM IR (.ll) to a file");
  app.add_flag("--compile-only", compileOnly,
               "Compile to object file (.o) without linking");
  app.add_option("-o,--output", outputFile, "Output file");
  app.add_option("--linker", linkerPath, "Linker path");
  app.add_option("--linker-arg", linkerArgs, "Linker arguments");

  // Subcommands
  blaze::cli::registerVerifyCommand(app, noColor, verbose);
  blaze::cli::registerBuildCommand(app, noColor, verbose);
  blaze::cli::registerEmitCommand(app, noColor, verbose);
  blaze::cli::registerRunCommand(app, noColor, verbose);

  CLI11_PARSE(app, argc, argv);

  bool usedSubcommand = false;
  for (auto *sub : app.get_subcommands()) {
    if (sub->parsed()) {
      usedSubcommand = true;
      break;
    }
  }

  if (!usedSubcommand) {
    if (emitLlvm && compileOnly) {
      fmt::print(
          stderr,
          "Error: --emit-llvm and --compile-only are mutually exclusive.\n");
      return 2;
    }

    auto extras = app.remaining();
    if (extras.empty()) {
      fmt::print(stderr, "Error: missing input file.\n");
      return 2;
    }
    if (extras.size() > 1) {
      fmt::print(stderr, "Error: too many arguments.\n");
      return 2;
    }

    std::string inputFile = extras.front();
    if (!std::filesystem::exists(inputFile)) {
      fmt::print(stderr, "Error: input file not found: {}\n", inputFile);
      return 2;
    }

    blaze::driver::CompileOptions opts;
    opts.inputFile = inputFile;
    opts.outputFile = outputFile;
    opts.linkerPath = linkerPath;
    opts.linkerArgs = linkerArgs;
    opts.noColor = noColor;
    opts.verbose = verbose;

    if (emitLlvm) {
      opts.outputKind = blaze::driver::OutputKind::EmitLLVM;
    } else if (compileOnly) {
      opts.outputKind = blaze::driver::OutputKind::EmitObject;
    } else {
      opts.outputKind = blaze::driver::OutputKind::EmitExecutable;
      // Default mode: build + verify.
      opts.verify = true;
    }

    auto result = blaze::driver::run(opts);

    blaze::core::PrintOptions printOpts;
    printOpts.useColor = !noColor;

    blaze::core::DiagnosticPrinter printer(printOpts);

    auto source = blaze::core::Source::fromFile(opts.inputFile);
    if (source) {
      fmt::print(stderr, "{}",
                 printer.format(result.diagnostics, &source.value()));
    } else {
      fmt::print(stderr, "{}", printer.format(result.diagnostics));
    }

    if (result.status != blaze::driver::DriverResult::Ok) {
      std::exit(result.status == blaze::driver::DriverResult::VerificationFailed
                    ? 1
                    : 2);
    }
  }

  return 0;
}
