#include <gtest/gtest.h>

#include "core/Source.h"
#include "frontend/FrontendDriver.h"

namespace blaze::frontend::tests {

namespace {

// Helper: run the parse → resolve pipeline and return whether it succeeded,
// together with the diagnostics for inspection.
struct ResolveOutcome {
  bool ok;
  core::DiagnosticList diagnostics;
};

ResolveOutcome resolve(const std::string &code,
                       const std::string &filename = "resolver_test.blz") {
  ResolveOutcome outcome;
  auto source = std::make_shared<core::Source>(code, filename);
  auto result = resolveSource(source, outcome.diagnostics);
  outcome.ok = result.has_value();
  return outcome;
}

} // namespace

// ---------------------------------------------------------------------------
// Valid programs – should resolve successfully
// ---------------------------------------------------------------------------

TEST(Resolver, ResolvesValidProgram) {
  auto [ok, diag] = resolve("fn foo(a: i32) -> i32 { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ResolvesOverloadedCall) {
  auto [ok, diag] = resolve("fn foo(a: i32) -> i32 { return a; }"
                            "fn foo(a: bool) -> bool { return a; }"
                            "fn bar() -> i32 { return foo(1); }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ResolvesBoolLiteral) {
  auto [ok, diag] = resolve("fn foo() -> bool { return true; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

// ---------------------------------------------------------------------------
// Unresolved symbols
// ---------------------------------------------------------------------------

TEST(Resolver, ReportsUnresolvedSymbol) {
  auto [ok, diag] = resolve("fn foo() -> i32 { return x; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(Resolver, ReportsUnresolvedType) {
  auto [ok, diag] = resolve("fn foo(a: unknown) -> i32 { return 1; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(Resolver, ReportsUnresolvedOverload) {
  auto [ok, diag] = resolve("fn foo() -> i32 { return bar(1); }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

// ---------------------------------------------------------------------------
// Duplicate / ambiguous declarations
// ---------------------------------------------------------------------------

TEST(Resolver, ReportsAmbiguousOverload) {
  auto [ok, diag] = resolve("fn foo(a: i32) -> i32 { return a; }"
                            "fn foo(a: i32) -> i32 { return a; }"
                            "fn bar() -> i32 { return foo(1); }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(Resolver, ReportsDuplicateVariable) {
  auto [ok, diag] =
      resolve("fn foo() -> i32 { var x: i32 = 1; var x: i32 = 2; return x; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

} // namespace blaze::frontend::tests
