#include "RunCommand.h"

#include <CLI/CLI.hpp>
#include <fmt/format.h>

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Program.h>

#include "core/DiagnosticPrinter.h"
#include "driver/Driver.h"

namespace blaze::cli {

void registerRunCommand(CLI::App &parent, bool &noColor, bool &verbose) {
  auto *cmd = parent.add_subcommand(
      "run", "Compile to a temporary executable and run it immediately");

  auto opts = std::make_shared<driver::CompileOptions>();
  opts->outputKind = driver::OutputKind::EmitExecutable;

  cmd->add_option("input", opts->inputFile, "Source file")
      ->required()
      ->check(CLI::ExistingFile);

  cmd->add_flag("--verify", opts->verify, "Run contract verification (Z3)");

  auto forwardedArgs = std::make_shared<std::vector<std::string>>();
  cmd->add_option("args", *forwardedArgs, "Arguments to pass to the program");

  cmd->callback([opts, forwardedArgs, &noColor, &verbose]() {
    opts->noColor = noColor;
    opts->verbose = verbose;

    llvm::SmallString<128> tempPath;
    if (std::error_code ec =
            llvm::sys::fs::createUniqueFile("blaze-run-%%%%%%.tmp", tempPath)) {
      fmt::print(stderr, "Failed to create temporary file: {}\n", ec.message());
      std::exit(1);
    }
    opts->outputFile = tempPath.str().str();

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
      if (std::error_code ec = llvm::sys::fs::remove(tempPath)) {
        // Ignore failure to remove temporary file
      }
      std::exit(result.status == driver::DriverResult::VerificationFailed ? 1
                                                                          : 2);
    }

    std::vector<llvm::StringRef> sysArgs;
    sysArgs.push_back(tempPath.str());
    for (const auto &arg : *forwardedArgs) {
      sysArgs.push_back(arg);
    }

    std::string errMsg;
    int exitCode = llvm::sys::ExecuteAndWait(tempPath.str(), sysArgs,
                                             llvm::None, {}, 0, 0, &errMsg);

    if (std::error_code ec = llvm::sys::fs::remove(tempPath)) {
      // Ignore failure to remove temporary file
    }

    if (exitCode < 0) {
      fmt::print(stderr, "Error executing program: {}\n", errMsg);
      std::exit(3);
    } else {
      std::exit(exitCode);
    }
  });
}

} // namespace blaze::cli
