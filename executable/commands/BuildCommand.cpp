#include "BuildCommand.h"

#include <CLI/CLI.hpp>
#include <fmt/format.h>

#include "core/DiagnosticPrinter.h"
#include "driver/Driver.h"

namespace blaze::cli {

void registerBuildCommand(CLI::App &parent, bool &noColor, bool &verbose) {
  auto *cmd = parent.add_subcommand("build", "Compile and link an executable");

  auto opts = std::make_shared<driver::CompileOptions>();
  opts->outputKind = driver::OutputKind::EmitExecutable;

  cmd->add_option("input", opts->inputFile, "Source file")
      ->required()
      ->check(CLI::ExistingFile);

  cmd->add_option("-o,--output", opts->outputFile, "Output file");
  cmd->add_flag("--verify", opts->verify, "Run contract verification (Z3)");
  cmd->add_option("--linker", opts->linkerPath, "Linker path");
  cmd->add_option("--linker-arg", opts->linkerArgs, "Linker arguments");

  cmd->callback([opts, &noColor, &verbose]() {
    opts->noColor = noColor;
    opts->verbose = verbose;

    auto result = driver::run(*opts);

    core::PrintOptions printOpts;
    printOpts.useColor = !noColor;

    core::DiagnosticPrinter printer(printOpts);

    auto source = core::Source::fromFile(opts->inputFile);
    if (source) {
      fmt::print(stderr, "{}",
                 printer.format(result.diagnostics, &source.value()));
    } else {
      fmt::print(stderr, "{}", printer.format(result.diagnostics));
    }

    if (result.status != driver::DriverResult::Ok) {
      std::exit(result.status == driver::DriverResult::VerificationFailed ? 1
                                                                          : 2);
    }
  });
}

} // namespace blaze::cli
