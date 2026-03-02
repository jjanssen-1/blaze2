#include "EmitCommand.h"

#include <CLI/CLI.hpp>
#include <fmt/format.h>

#include "core/DiagnosticPrinter.h"
#include "driver/Driver.h"

namespace blaze::cli {

void registerEmitCommand(CLI::App &parent, bool &noColor, bool &verbose) {
  auto *emitIrCmd =
      parent.add_subcommand("emit-ir", "Print Blaze SSA IR to stdout");

  auto irOpts = std::make_shared<driver::CompileOptions>();
  irOpts->outputKind = driver::OutputKind::PrintIR;

  emitIrCmd->add_option("input", irOpts->inputFile, "Source file")
      ->required()
      ->check(CLI::ExistingFile);

  emitIrCmd->callback([irOpts, &noColor, &verbose]() {
    irOpts->noColor = noColor;
    irOpts->verbose = verbose;

    auto result = driver::run(*irOpts);

    core::PrintOptions printOpts;
    printOpts.useColor = !noColor;
    core::DiagnosticPrinter printer(printOpts);

    auto source = core::Source::fromFile(irOpts->inputFile);
    if (source) {
      fmt::print(stderr, "{}",
                 printer.format(result.diagnostics, &source.value()));
    } else {
      fmt::print(stderr, "{}", printer.format(result.diagnostics));
    }

    if (result.status == driver::DriverResult::Ok && result.irText) {
      fmt::print("{}", *result.irText);
    } else if (result.status != driver::DriverResult::Ok) {
      std::exit(result.status == driver::DriverResult::VerificationFailed ? 1
                                                                          : 2);
    }
  });

  auto *emitLlvmCmd =
      parent.add_subcommand("emit-llvm", "Write LLVM IR (.ll) to a file");

  auto llvmOpts = std::make_shared<driver::CompileOptions>();
  llvmOpts->outputKind = driver::OutputKind::EmitLLVM;

  emitLlvmCmd->add_option("input", llvmOpts->inputFile, "Source file")
      ->required()
      ->check(CLI::ExistingFile);

  emitLlvmCmd->add_option("-o,--output", llvmOpts->outputFile, "Output file");

  emitLlvmCmd->callback([llvmOpts, &noColor, &verbose]() {
    llvmOpts->noColor = noColor;
    llvmOpts->verbose = verbose;

    auto result = driver::run(*llvmOpts);

    core::PrintOptions printOpts;
    printOpts.useColor = !noColor;
    core::DiagnosticPrinter printer(printOpts);

    auto source = core::Source::fromFile(llvmOpts->inputFile);
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
