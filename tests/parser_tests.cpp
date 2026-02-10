#include <gtest/gtest.h>

#include "core/Source.h"
#include "frontend/FrontendDriver.h"

namespace blaze::frontend::tests {

namespace {

// Helper: run the parse pipeline and return whether it succeeded, the AST root,
// and the diagnostics for inspection.
struct ParseOutcome {
  bool ok;
  std::shared_ptr<Root> root;
  core::DiagnosticList diagnostics;
};

ParseOutcome parse(const std::string &code,
                   const std::string &filename = "parser_test.blz") {
  ParseOutcome outcome;
  auto source = std::make_shared<core::Source>(code, filename);
  auto result = parseSource(source, outcome.diagnostics);
  outcome.root = result.has_value() ? result.value().root : nullptr;
  outcome.ok =
      result.has_value() && outcome.root && !outcome.diagnostics.hasErrors();
  return outcome;
}

} // namespace

// ---------------------------------------------------------------------------
// Valid programs – should parse successfully
// ---------------------------------------------------------------------------

TEST(ParserDriver, ParsesValidProgram) {
  auto [ok, root, diag] = parse("fn foo() { var x: int = 1; return x; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(root);
  EXPECT_TRUE(diag.empty());
}

// ---------------------------------------------------------------------------
// Invalid programs – should fail to parse
// ---------------------------------------------------------------------------

TEST(ParserDriver, RejectsInvalidProgram) {
  auto [ok, root, diag] = parse("fn foo( { return 1; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(root);
  EXPECT_FALSE(diag.empty());
}

} // namespace blaze::frontend::tests
