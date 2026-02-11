#include <gtest/gtest.h>

#include "core/Errors.h"
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

// ---------------------------------------------------------------------------
// Pre/post specifications — parameter visibility
// ---------------------------------------------------------------------------

TEST(Resolver, ResolvesParameterInPrecondition) {
  auto [ok, diag] = resolve("fn foo(a: i32) -> i32 pre { a; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ResolvesParameterInPostcondition) {
  auto [ok, diag] = resolve("fn foo(a: i32) -> i32 post { a; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ResolvesParameterInBothPreAndPost) {
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 pre { a; } post { a; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ReportsUnresolvedSymbolInPrecondition) {
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 pre { unknown; } { return a; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(Resolver, ReportsUnresolvedSymbolInPostcondition) {
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 post { unknown; } { return a; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

// ---------------------------------------------------------------------------
// Post result binding — post(r) { ... }
// ---------------------------------------------------------------------------

TEST(Resolver, ResolvesResultBindingInPostcondition) {
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 post { r : r; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ResolvesResultBindingAlongsideParameter) {
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 post { r : r; a; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ResultBindingNotVisibleInPrecondition) {
  // The name "r" is introduced by post(r), not pre. Using "r" in pre
  // should fail to resolve (assuming no other "r" in scope).
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 pre { r; } post { r : r; } { return a; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(Resolver, ResultBindingNotVisibleInFunctionBody) {
  // "r" from post(r) should not leak into the function body.
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 post { r : r; } { return r; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(Resolver, ResultBindingOnVoidFunctionReportsError) {
  auto [ok, diag] = resolve("fn foo(a: i32) post { r : a; } { a; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
  bool foundError = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_RESULT_BINDING_ON_VOID) {
      foundError = true;
      break;
    }
  }
  EXPECT_TRUE(foundError);
}

TEST(Resolver, ResultBindingWithCustomName) {
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 post { result : result; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(Resolver, ResultBindingDoesNotShadowParameter) {
  // If the result binding has the same name as a parameter, the result
  // binding (in an inner scope) should shadow the parameter within post.
  // This is valid — no duplicate error should occur.
  auto [ok, diag] =
      resolve("fn foo(a: i32) -> i32 post { a : a; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

} // namespace blaze::frontend::tests
