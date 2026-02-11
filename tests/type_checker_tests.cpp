#include <gtest/gtest.h>

#include "core/Errors.h"
#include "core/Source.h"
#include "frontend/FrontendDriver.h"

namespace blaze::frontend::tests {

namespace {

// Helper: run the full pipeline (parse → resolve → type-check) and return
// whether it succeeded, together with the diagnostics for inspection.
struct CheckOutcome {
  bool ok;
  core::DiagnosticList diagnostics;
};

CheckOutcome check(const std::string &code,
                   const std::string &filename = "typecheck_test.blz") {
  CheckOutcome outcome;
  auto source = std::make_shared<core::Source>(code, filename);
  auto result = checkSource(source, outcome.diagnostics);
  outcome.ok = result.has_value();
  return outcome;
}

} // namespace

// ---------------------------------------------------------------------------
// Valid programs – should pass type checking without errors
// ---------------------------------------------------------------------------

TEST(TypeChecker, AcceptsValidReturnI32) {
  auto [ok, diag] = check("fn foo(a: i32) -> i32 { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsValidReturnBool) {
  auto [ok, diag] = check("fn foo() -> bool { return true; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsValidDeclaration) {
  auto [ok, diag] = check("fn foo() -> i32 { var x: i32 = 42; return x; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsValidBoolDeclaration) {
  auto [ok, diag] =
      check("fn foo() -> bool { var b: bool = false; return b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsVoidFunctionNoReturn) {
  // A void function (no return-type annotation) with no return statement.
  auto [ok, diag] = check("fn foo(a: i32) { a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsBinaryExprSameTypes) {
  auto [ok, diag] = check("fn foo() -> i32 { return 1 + 2; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsIfWithBoolCondition) {
  auto [ok, diag] =
      check("fn foo(a: bool) -> i32 { if (a) { return 1; } return 0; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsWhileWithBoolCondition) {
  auto [ok, diag] =
      check("fn foo(a: bool) -> i32 { while (a) { a; } return 0; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsCallExpressionMatchingTypes) {
  auto [ok, diag] = check("fn bar(x: i32) -> i32 { return x; }"
                          "fn foo() -> i32 { return bar(1); }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsMultipleFunctions) {
  auto [ok, diag] =
      check("fn a() -> i32 { return 1; }"
            "fn b() -> bool { return true; }"
            "fn c(x: i32) -> i32 { var y: i32 = x + 1; return y; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

// ---------------------------------------------------------------------------
// Return-type mismatches
// ---------------------------------------------------------------------------

TEST(TypeChecker, RejectsReturnTypeMismatch) {
  // Function declares i32 return but returns a bool.
  auto [ok, diag] = check("fn foo() -> i32 { return true; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(TypeChecker, RejectsReturnBoolWhenI32Expected) {
  auto [ok, diag] = check("fn foo() -> bool { return 42; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

// ---------------------------------------------------------------------------
// Declaration-type mismatches
// ---------------------------------------------------------------------------

TEST(TypeChecker, RejectsDeclTypeMismatchBoolGotInt) {
  auto [ok, diag] = check("fn foo() -> i32 { var x: bool = 1; return 0; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(TypeChecker, RejectsDeclTypeMismatchIntGotBool) {
  auto [ok, diag] = check("fn foo() -> i32 { var x: i32 = true; return x; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

// ---------------------------------------------------------------------------
// Condition must be boolean
// ---------------------------------------------------------------------------

TEST(TypeChecker, RejectsIfConditionNotBool) {
  // Using an integer expression as a condition should fail.
  auto [ok, diag] =
      check("fn foo(a: i32) -> i32 { if (a) { return 1; } return 0; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(TypeChecker, RejectsWhileConditionNotBool) {
  auto [ok, diag] =
      check("fn foo(a: i32) -> i32 { while (a) { a; } return 0; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

// ---------------------------------------------------------------------------
// Multiple errors in a single program
// ---------------------------------------------------------------------------

TEST(TypeChecker, ReportsMultipleErrors) {
  // Two independent type errors: bad declaration and bad return.
  auto [ok, diag] = check("fn foo() -> i32 { var x: bool = 1; return true; }");
  EXPECT_FALSE(ok);
  // At least two diagnostics expected (decl mismatch + return mismatch).
  EXPECT_GE(diag.size(), 2u);
}

// ---------------------------------------------------------------------------
// Nested constructs
// ---------------------------------------------------------------------------

TEST(TypeChecker, AcceptsNestedIfElse) {
  auto [ok, diag] = check("fn foo(a: bool, b: bool) -> i32 {"
                          "  if (a) { if (b) { return 1; } return 2; }"
                          "  return 0;"
                          "}");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsNestedReturnMismatch) {
  auto [ok, diag] = check("fn foo(a: bool) -> i32 {"
                          "  if (a) { return true; }"
                          "  return 0;"
                          "}");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

// ---------------------------------------------------------------------------
// Assignment checks
// ---------------------------------------------------------------------------

TEST(TypeChecker, AcceptsValidVarAssignment) {
  auto [ok, diag] =
      check("fn foo() -> i32 { var x: i32 = 1; x = 2; return x; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsAssignToConst) {
  auto [ok, diag] =
      check("fn foo() -> i32 { const x: i32 = 1; x = 2; return x; }");
  EXPECT_FALSE(ok);
  ASSERT_FALSE(diag.empty());
  bool foundConstError = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_ASSIGN_TO_CONST) {
      foundConstError = true;
      break;
    }
  }
  EXPECT_TRUE(foundConstError);
}

TEST(TypeChecker, RejectsAssignToParameter) {
  auto [ok, diag] = check("fn foo(x: i32) -> i32 { x = 2; return x; }");
  EXPECT_FALSE(ok);
  ASSERT_FALSE(diag.empty());
  bool foundConstError = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_ASSIGN_TO_CONST) {
      foundConstError = true;
      break;
    }
  }
  EXPECT_TRUE(foundConstError);
}

TEST(TypeChecker, RejectsAssignmentTypeMismatch) {
  auto [ok, diag] =
      check("fn foo() -> i32 { var x: i32 = 1; x = true; return x; }");
  EXPECT_FALSE(ok);
  ASSERT_FALSE(diag.empty());
  bool foundMismatch = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_TYPE_MISMATCH) {
      foundMismatch = true;
      break;
    }
  }
  EXPECT_TRUE(foundMismatch);
}

// ---------------------------------------------------------------------------
// Pre/post specification type checking
// ---------------------------------------------------------------------------

TEST(TypeChecker, AcceptsPreWithBoolExpression) {
  auto [ok, diag] = check("fn foo(a: bool) -> i32 pre { a; } { return 0; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsPreWithNonBoolExpression) {
  auto [ok, diag] = check("fn foo(a: i32) -> i32 pre { a; } { return 0; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

TEST(TypeChecker, AcceptsPostWithBoolExpression) {
  auto [ok, diag] = check("fn foo(a: bool) -> i32 post { a; } { return 0; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsPostWithNonBoolExpression) {
  auto [ok, diag] = check("fn foo(a: i32) -> i32 post { a; } { return 0; }");
  EXPECT_FALSE(ok);
  EXPECT_FALSE(diag.empty());
}

// ---------------------------------------------------------------------------
// Post result binding — post(r) { ... }
// ---------------------------------------------------------------------------

TEST(TypeChecker, AcceptsPostResultBindingWithBoolExpr) {
  // post(r) where r is i32; the expression must still be bool.
  // Here we use a boolean literal that references r for side effect.
  auto [ok, diag] = check("fn foo(a: bool) -> i32 post {r: a; } { return 0; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsPostResultBindingNonBoolExpr) {
  // r is i32, using r directly as the postcondition should fail
  // because r is not bool.
  auto [ok, diag] = check("fn foo(a: i32) -> i32 post {r: r; } { return a; }");
  EXPECT_FALSE(ok);
  bool foundMismatch = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_TYPE_MISMATCH) {
      foundMismatch = true;
      break;
    }
  }
  EXPECT_TRUE(foundMismatch);
}

TEST(TypeChecker, AcceptsPostResultBindingBoolReturn) {
  // Function returns bool, so r is bool — using r directly is valid.
  auto [ok, diag] =
      check("fn foo(a: bool) -> bool post {r: r; } { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsPostResultBindingOnVoidFunction) {
  auto [ok, diag] = check("fn foo(a: bool) post {r: a; } { a; }");
  EXPECT_FALSE(ok);
  bool foundError = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_RESULT_BINDING_ON_VOID) {
      foundError = true;
      break;
    }
  }
  EXPECT_TRUE(foundError);
}

TEST(TypeChecker, AcceptsPostWithoutResultBinding) {
  // Plain post { ... } on a non-void function — no result binding, should work.
  auto [ok, diag] = check("fn foo(a: bool) -> i32 post { a; } { return 0; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsPreAndPostResultBindingTogether) {
  auto [ok, diag] = check("fn foo(a: bool) -> bool "
                          "pre { a; } "
                          "post {r: r; } "
                          "{ return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

// ---------------------------------------------------------------------------
// Comparison operators
// ---------------------------------------------------------------------------

TEST(TypeChecker, AcceptsLessThanSameTypes) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> bool { return a < b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsLessEqualSameTypes) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> bool { return a <= b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsGreaterThanSameTypes) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> bool { return a > b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsGreaterEqualSameTypes) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> bool { return a >= b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsEqualSameTypes) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> bool { return a == b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsNotEqualSameTypes) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> bool { return a != b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsBoolEqualityComparison) {
  auto [ok, diag] =
      check("fn foo(a: bool, b: bool) -> bool { return a == b; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsComparisonMixedTypes) {
  // Comparing i32 with bool should be a type mismatch.
  auto [ok, diag] = check("fn foo(a: i32, b: bool) -> bool { return a < b; }");
  EXPECT_FALSE(ok);
  bool foundMismatch = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_TYPE_MISMATCH) {
      foundMismatch = true;
      break;
    }
  }
  EXPECT_TRUE(foundMismatch);
}

TEST(TypeChecker, RejectsComparisonResultAsI32) {
  // A comparison produces bool; assigning it to i32 should fail.
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> i32 { return a < b; }");
  EXPECT_FALSE(ok);
  bool foundMismatch = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_TYPE_MISMATCH) {
      foundMismatch = true;
      break;
    }
  }
  EXPECT_TRUE(foundMismatch);
}

TEST(TypeChecker, AcceptsComparisonAsIfCondition) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> i32 {"
                          "  if (a < b) { return a; }"
                          "  return b;"
                          "}");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsComparisonAsWhileCondition) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> i32 {"
                          "  while (a < b) { return a; }"
                          "  return b;"
                          "}");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsComparisonInPrecondition) {
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> i32 "
                          "pre { a < b; } "
                          "{ return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsComparisonInPostconditionWithResultBinding) {
  auto [ok, diag] = check("fn foo(a: i32) -> i32 "
                          "post {r: r >= a; } "
                          "{ return a; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, AcceptsComparisonDeclBool) {
  // Storing a comparison result in a bool variable.
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> bool {"
                          "  var result: bool = a == b;"
                          "  return result;"
                          "}");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

TEST(TypeChecker, RejectsComparisonDeclI32) {
  // Storing a comparison result in an i32 variable should fail.
  auto [ok, diag] = check("fn foo(a: i32, b: i32) -> i32 {"
                          "  var result: i32 = a == b;"
                          "  return result;"
                          "}");
  EXPECT_FALSE(ok);
  bool foundMismatch = false;
  for (const auto &d : diag) {
    if (d.errorCode == core::ERROR_TYPE_MISMATCH) {
      foundMismatch = true;
      break;
    }
  }
  EXPECT_TRUE(foundMismatch);
}

TEST(TypeChecker, AcceptsComparisonWithArithmeticOperands) {
  // Arithmetic sub-expressions fed into a comparison.
  auto [ok, diag] =
      check("fn foo(a: i32, b: i32) -> bool { return a + 1 <= b * 2; }");
  EXPECT_TRUE(ok);
  EXPECT_TRUE(diag.empty());
}

} // namespace blaze::frontend::tests
