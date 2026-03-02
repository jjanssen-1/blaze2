#include "VerifyCommand.h"

#include <CLI/CLI.hpp>
#include <fmt/format.h>

#include "core/DiagnosticPrinter.h"
#include "driver/Driver.h"

namespace blaze::cli {

void registerVerifyCommand(CLI::App &parent, bool &noColor, bool &verbose) {
  auto *cmd = parent.add_subcommand("verify", "Verify contracts with Z3");

  auto opts = std::make_shared<driver::CompileOptions>();
  opts->verifyOnly = true;
  opts->verify = true;

  cmd->add_option("input", opts->inputFile, "Source file")
      ->required()
      ->check(CLI::ExistingFile);

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

    if (result.status == driver::DriverResult::Ok) {
      fmt::print("All contracts verified.\n");
    }

    if (result.status != driver::DriverResult::Ok) {
      std::exit(result.status == driver::DriverResult::VerificationFailed ? 1
                                                                          : 2);
    }
  });
}

} // namespace blaze::cli
