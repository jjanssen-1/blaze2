#include <gtest/gtest.h>

#include "BlazeLexer.h"
#include "core/Source.h"
#include "frontend/FrontendDriver.h"

#include <antlr4-runtime.h>

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

TEST(ParserDriver, TokenOffsetsMatchSourceSlices) {
  const std::string code = "/* π */ fn foo() { var x: i32 = 1; return x; }";
  core::Source source(code, "offset_test.blz");

  antlr4::ANTLRInputStream input(std::string(source.text()));
  BlazeLexer lexer(&input);
  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  for (auto *token : tokens.getTokens()) {
    if (token->getType() == antlr4::Token::EOF)
      continue;

    const auto start = token->getStartIndex();
    const auto stop = token->getStopIndex();
    ASSERT_GE(start, 0);
    ASSERT_GE(stop, start);

    const auto offset =
        source.byteOffsetForCodepoint(static_cast<core::size>(start));
    const auto length = source.byteLengthForCodepointRange(
        static_cast<core::size>(start), static_cast<core::size>(stop));
    const auto slice = source.view(offset, length).text();

    EXPECT_EQ(slice, token->getText()) << "Token type " << token->getType();
  }
}

} // namespace blaze::frontend::tests
