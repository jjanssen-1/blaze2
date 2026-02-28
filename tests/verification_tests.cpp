#include <gtest/gtest.h>

#include "core/Errors.h"
#include "core/Source.h"

#include "frontend/Builtins.h"
#include "frontend/FrontendDriver.h"
#include "frontend/Ir.h"
#include "frontend/IrLower.h"
#include "frontend/Resolver.h"
#include "frontend/SymbolTable.h"

#include "backend/VcBackend.h"
#include "backend/z3/z3backend.h"

namespace blaze::verification::tests {

namespace {

struct VerifyOutcome {
  bool ok;
  core::DiagnosticList diagnostics;
  std::vector<frontend::IRFunction> functions;
  backend::VcBackend::VerificationResult result;
};

VerifyOutcome verify(const std::string &code,
                     const std::string &filename = "verify_test.blz") {
  VerifyOutcome outcome;
  auto source = std::make_shared<core::Source>(code, filename);

  auto checked = frontend::checkSource(source, outcome.diagnostics);
  if (!checked) {
    outcome.ok = false;
    outcome.result.status = backend::VcBackend::Status::Error;
    return outcome;
  }

  auto symbolTable =
      std::make_shared<frontend::SymbolTable>(checked->resolve.symbols);
  auto builtins =
      std::make_shared<frontend::BuiltinRegistry>(checked->resolve.builtins);

  frontend::IRBuilder builder(outcome.diagnostics);
  outcome.functions = builder.lower(checked->parse.root, symbolTable, builtins);

  if (outcome.diagnostics.hasErrors()) {
    outcome.ok = false;
    outcome.result.status = backend::VcBackend::Status::Error;
    return outcome;
  }

  backend::Z3Backend z3;
  outcome.result = z3.verify(outcome.functions);
  outcome.ok = true;
  return outcome;
}

bool isVerified(const VerifyOutcome &v) {
  return v.ok && v.result.status == backend::VcBackend::Status::Verified;
}

bool isDisproven(const VerifyOutcome &v) {
  return v.ok && v.result.status == backend::VcBackend::Status::Disproven;
}

} // namespace

// ===========================================================================
// Basic pre/post contract verification (no loops)
// ===========================================================================

TEST(Verify, SimplePostconditionVerified) {
  auto v = verify("fn id(x: i32) -> i32"
                  "  post { r: r == x; }"
                  "{"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v)) << "Identity function postcondition should verify";
}

TEST(Verify, IncorrectPostconditionDisproven) {
  auto v = verify("fn bad(x: i32) -> i32"
                  "  post { r: r == x + 1; }"
                  "{"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isDisproven(v)) << "Returning x should disprove r == x + 1";
}

TEST(Verify, PreconditionHelpsVerify) {
  auto v = verify("fn non_neg(x: i32) -> i32"
                  "  pre { x >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, ArithmeticPostcondition) {
  auto v = verify("fn add_one(x: i32) -> i32"
                  "  post { r: r == x + 1; }"
                  "{"
                  "  return x + 1;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, MultiplePostconditions) {
  auto v = verify("fn clamp_low(x: i32) -> i32"
                  "  pre { x >= 0; }"
                  "  post { r: r >= 0; r >= x; }"
                  "{"
                  "  return x + 1;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, IfElsePostcondition) {
  auto v = verify("fn abs_ish(x: i32) -> i32"
                  "  pre { x > 0 - 100; x < 100; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  if (x >= 0) {"
                  "    return x;"
                  "  }"
                  "  return 0 - x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, NegationPostcondition) {
  auto v = verify("fn nonzero(x: i32) -> i32"
                  "  pre { x != 0; }"
                  "  post { r: !(r == 0); }"
                  "{"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, MultipleReturnSitesAllSatisfyPost) {
  auto v = verify("fn bounded(x: i32) -> i32"
                  "  pre { x > 0 - 50; x < 50; }"
                  "  post { r: r >= 0; r < 50; }"
                  "{"
                  "  if (x >= 0) {"
                  "    return x;"
                  "  }"
                  "  return 0 - x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, VoidFunctionWithPrePost) {
  auto v = verify("fn void_ok(x: i32)"
                  "  pre { x > 0; }"
                  "  post { x > 0; }"
                  "{"
                  "  x + 1;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

// ===========================================================================
// Call-site contract verification
// ===========================================================================

TEST(Verify, CallerSatisfiesCalleePrecondition) {
  auto v = verify("fn callee(x: i32) -> i32"
                  "  pre { x > 0; }"
                  "  post { r: r > 0; }"
                  "{"
                  "  return x;"
                  "}"
                  "fn caller() -> i32 {"
                  "  return callee(42);"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, CallerViolatesCalleePrecondition) {
  auto v = verify("fn callee(x: i32) -> i32"
                  "  pre { x > 0; }"
                  "  post { r: r > 0; }"
                  "{"
                  "  return x;"
                  "}"
                  "fn caller() -> i32 {"
                  "  return callee(0);"
                  "}");
  EXPECT_TRUE(isDisproven(v)) << "Calling callee(0) violates pre { x > 0 }";
}

TEST(Verify, ChainedCallsVerify) {
  auto v = verify("fn inc(x: i32) -> i32"
                  "  pre { x >= 0; }"
                  "  post { r: r > x; }"
                  "{"
                  "  return x + 1;"
                  "}"
                  "fn double_inc(x: i32) -> i32"
                  "  pre { x >= 0; }"
                  "  post { r: r > x; }"
                  "{"
                  "  var y: i32 = inc(x);"
                  "  return inc(y);"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

// ===========================================================================
// Loop invariant verification — verified cases
// ===========================================================================

TEST(Verify, SimpleCountdownInvariant) {
  auto v = verify("fn countdown(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r == 0; }"
                  "{"
                  "  var i: i32 = n;"
                  "  while (i > 0)"
                  "    invariant { i >= 0; }"
                  "  {"
                  "    i = i - 1;"
                  "  }"
                  "  return i;"
                  "}");
  EXPECT_TRUE(isVerified(v))
      << "Countdown with invariant i >= 0 should verify r == 0";
}

TEST(Verify, AccumulatorInvariant) {
  auto v = verify("fn sum_nonneg(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  var acc: i32 = 0;"
                  "  var i: i32 = 0;"
                  "  while (i < n)"
                  "    invariant { acc >= 0; i >= 0; i <= n; }"
                  "  {"
                  "    acc = acc + i;"
                  "    i = i + 1;"
                  "  }"
                  "  return acc;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, BoundedCounterInvariant) {
  auto v = verify("fn count_to_five() -> i32"
                  "  post { r: r == 5; }"
                  "{"
                  "  var i: i32 = 0;"
                  "  while (i < 5)"
                  "    invariant { i >= 0; i <= 5; }"
                  "  {"
                  "    i = i + 1;"
                  "  }"
                  "  return i;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, IdentityLoopInvariant) {
  // A loop that does not modify x; invariant says x == n.
  auto v = verify("fn identity(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r == n; }"
                  "{"
                  "  var x: i32 = n;"
                  "  var i: i32 = 0;"
                  "  while (i < 10)"
                  "    invariant { x == n; i >= 0; i <= 10; }"
                  "  {"
                  "    i = i + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, SequentialLoopsInvariant) {
  auto v = verify("fn two_loops(a: i32, b: i32) -> i32"
                  "  pre { a >= 0; b >= 0; }"
                  "  post { r: r == 0; }"
                  "{"
                  "  var x: i32 = a;"
                  "  while (x > 0)"
                  "    invariant { x >= 0; }"
                  "  {"
                  "    x = x - 1;"
                  "  }"
                  "  var y: i32 = b;"
                  "  while (y > 0)"
                  "    invariant { y >= 0; x == 0; }"
                  "  {"
                  "    y = y - 1;"
                  "  }"
                  "  return x + y;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, SequentialLoopsReuseSameVariable) {
  auto v = verify("fn reuse_loop_var(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r == 0; }"
                  "{"
                  "  var i: i32 = n;"
                  "  while (i > 0)"
                  "    invariant { i >= 0; }"
                  "  {"
                  "    i = i - 1;"
                  "  }"
                  "  i = n;"
                  "  while (i > 0)"
                  "    invariant { i >= 0; }"
                  "  {"
                  "    i = i - 1;"
                  "  }"
                  "  return i;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, NeverExecutingLoopWeakInvariant) {
  // With Hoare-logic reasoning, post-loop state is I ∧ ¬B.
  // Here B is `false`, so ¬B is `true`, and we only get I: x >= 0.
  // That's enough to prove r >= 0 but NOT r == 0.
  auto v = verify("fn never_runs() -> i32"
                  "  post { r: r >= 0; }"
                  "{"
                  "  var x: i32 = 0;"
                  "  while (false)"
                  "    invariant { x >= 0; }"
                  "  {"
                  "    x = x + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, NeverExecutingLoopStrongInvariant) {
  // A stronger invariant (x == 0) lets us prove the exact postcondition.
  // The invariant is established (x starts at 0), trivially preserved
  // (body adds 1, but the preservation check uses abstract state where
  // x == 0 && false is unsat — the body is unreachable), and gives
  // x == 0 after the loop.
  auto v = verify("fn never_runs_strong() -> i32"
                  "  post { r: r == 0; }"
                  "{"
                  "  var x: i32 = 0;"
                  "  while (false)"
                  "    invariant { x == 0; }"
                  "  {"
                  "    x = x + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, InvariantPreservesMonotonicity) {
  // x starts at n and only increases; invariant says x >= n.
  auto v = verify("fn grow(n: i32) -> i32"
                  "  pre { n >= 1; n < 100; }"
                  "  post { r: r >= n; }"
                  "{"
                  "  var x: i32 = n;"
                  "  var i: i32 = 0;"
                  "  while (i < 3)"
                  "    invariant { x >= n; i >= 0; i <= 3; }"
                  "  {"
                  "    x = x + n;"
                  "    i = i + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, SwapTrackInvariant) {
  auto v = verify("fn swap_track(a: i32, b: i32) -> i32"
                  "  pre { a >= 0; b >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  var x: i32 = a;"
                  "  var y: i32 = b;"
                  "  while (x > 0)"
                  "    invariant { x >= 0; y >= 0; }"
                  "  {"
                  "    y = y + 1;"
                  "    x = x - 1;"
                  "  }"
                  "  return y;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, VoidFunctionWithInvariant) {
  auto v = verify("fn void_loop(n: i32)"
                  "  pre { n >= 0; }"
                  "{"
                  "  var i: i32 = 0;"
                  "  while (i < n)"
                  "    invariant { i >= 0; i <= n; }"
                  "  {"
                  "    i = i + 1;"
                  "  }"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, MultiInvariantExpressions) {
  auto v = verify("fn multi(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r >= 0; r <= n; }"
                  "{"
                  "  var x: i32 = 0;"
                  "  while (x < n)"
                  "    invariant { x >= 0; x <= n; }"
                  "  {"
                  "    x = x + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

// ===========================================================================
// Loop invariant verification — disproven / failing cases
// ===========================================================================

TEST(Verify, InvariantNotEstablished) {
  // Invariant says x >= 0 but x starts at -1.
  auto v = verify("fn bad_init() -> i32 {"
                  "  var x: i32 = 0 - 1;"
                  "  while (x < 5)"
                  "    invariant { x >= 0; }"
                  "  {"
                  "    x = x + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isDisproven(v))
      << "Invariant x >= 0 is not established (x starts at -1)";
}

TEST(Verify, InvariantNotPreserved) {
  // Invariant says i >= 0 but the body decrements past zero.
  auto v = verify("fn bad_preserve(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "{"
                  "  var i: i32 = n;"
                  "  while (i > 0 - 10)"
                  "    invariant { i >= 0; }"
                  "  {"
                  "    i = i - 1;"
                  "  }"
                  "  return i;"
                  "}");
  EXPECT_TRUE(isDisproven(v)) << "Invariant i >= 0 is not preserved when "
                                 "condition allows i to go negative";
}

TEST(Verify, PostconditionFailsWithoutInvariant) {
  // Without an invariant, the solver cannot prove the postcondition
  // because loop-carried state is unconstrained.
  auto v = verify("fn no_inv(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r == 0; }"
                  "{"
                  "  var i: i32 = n;"
                  "  while (i > 0) {"
                  "    i = i - 1;"
                  "  }"
                  "  return i;"
                  "}");
  // Without an invariant, the verifier should fail to prove this
  // (the loop-carried i is unconstrained).
  EXPECT_TRUE(isDisproven(v))
      << "Without invariant, solver should fail to verify post { r == 0 }";
}

TEST(Verify, InvariantTooWeak) {
  // Invariant is true but too weak to prove the postcondition.
  auto v = verify("fn weak(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r == 0; }"
                  "{"
                  "  var i: i32 = n;"
                  "  while (i > 0)"
                  "    invariant { i >= 0 - 100; }"
                  "  {"
                  "    i = i - 1;"
                  "  }"
                  "  return i;"
                  "}");
  // i >= -100 && !(i > 0) only gives i <= 0 && i >= -100, not i == 0.
  EXPECT_TRUE(isDisproven(v))
      << "Invariant i >= -100 is too weak to prove r == 0";
}

TEST(Verify, WrongInvariantExpression) {
  // Invariant claims i <= 5 but the loop goes up to n which can be > 5.
  auto v = verify("fn wrong(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "{"
                  "  var i: i32 = 0;"
                  "  while (i < n)"
                  "    invariant { i <= 5; }"
                  "  {"
                  "    i = i + 1;"
                  "  }"
                  "  return i;"
                  "}");
  EXPECT_TRUE(isDisproven(v))
      << "Invariant i <= 5 fails preservation when n > 5";
}

// ===========================================================================
// Nested loops with invariants
// ===========================================================================

TEST(Verify, NestedLoopInvariantsVerified) {
  auto v = verify("fn nested(rows: i32, cols: i32) -> i32"
                  "  pre { rows >= 0; cols >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  var total: i32 = 0;"
                  "  var r: i32 = 0;"
                  "  while (r < rows)"
                  "    invariant { total >= 0; r >= 0; r <= rows; }"
                  "  {"
                  "    var c: i32 = 0;"
                  "    while (c < cols)"
                  "      invariant { c >= 0; c <= cols; total >= 0; }"
                  "    {"
                  "      total = total + 1;"
                  "      c = c + 1;"
                  "    }"
                  "    r = r + 1;"
                  "  }"
                  "  return total;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, NestedLoopsMutateSharedVariable) {
  auto v = verify("fn shared_mut(n: i32, m: i32) -> i32"
                  "  pre { n >= 0; m >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  var x: i32 = 0;"
                  "  var i: i32 = 0;"
                  "  while (i < n)"
                  "    invariant { x >= 0; i >= 0; i <= n; }"
                  "  {"
                  "    var j: i32 = 0;"
                  "    while (j < m)"
                  "      invariant { x >= 0; j >= 0; j <= m; i >= 0; i <= n; }"
                  "    {"
                  "      x = x + 1;"
                  "      j = j + 1;"
                  "    }"
                  "    i = i + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, NestedLoopsSharedVariableInvariantNotPreserved) {
  auto v = verify("fn shared_bad(n: i32, m: i32) -> i32"
                  "  pre { n >= 0; m >= 0; }"
                  "{"
                  "  var x: i32 = 0;"
                  "  var i: i32 = 0;"
                  "  while (i < n)"
                  "    invariant { x >= 0; i >= 0; i <= n; }"
                  "  {"
                  "    var j: i32 = 0;"
                  "    while (j < m)"
                  "      invariant { x >= 0; j >= 0; j <= m; i >= 0; i <= n; }"
                  "    {"
                  "      x = x - 1;"
                  "      j = j + 1;"
                  "    }"
                  "    i = i + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isDisproven(v))
      << "Inner loop decrements x, violating invariant x >= 0";
}

TEST(Verify, OuterLoopInvariantNotEstablishedDueToInnerMutation) {
  auto v = verify("fn shared_init_bad(n: i32, m: i32) -> i32"
                  "  pre { n >= 0; m >= 0; }"
                  "{"
                  "  var x: i32 = 0;"
                  "  var i: i32 = 0;"
                  "  while (i < n)"
                  "    invariant { x >= 0; i >= 0; i <= n; }"
                  "  {"
                  "    x = x - 1;"
                  "    var j: i32 = 0;"
                  "    while (j < m)"
                  "      invariant { j >= 0; j <= m; i >= 0; i <= n; }"
                  "    {"
                  "      j = j + 1;"
                  "    }"
                  "    i = i + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isDisproven(v))
      << "Outer loop invariant x >= 0 is not preserved after x is decremented";
}

// ===========================================================================
// Edge cases and solver capability exploration
// ===========================================================================

TEST(Verify, EmptyInvariantClauseVerifies) {
  // Empty invariant clause: behaves as if no invariant was given.
  auto v = verify("fn empty_inv(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "{"
                  "  var i: i32 = n;"
                  "  while (i > 0)"
                  "    invariant { }"
                  "  {"
                  "    i = i - 1;"
                  "  }"
                  "  return i;"
                  "}");
  // No checks, no proof obligations from invariant — should still compile.
  EXPECT_TRUE(v.ok);
}

TEST(Verify, InvariantWithConstantTrue) {
  auto v = verify("fn trivial_inv() -> i32"
                  "  post { r: r == 5; }"
                  "{"
                  "  var i: i32 = 0;"
                  "  while (i < 5)"
                  "    invariant { true; i >= 0; i <= 5; }"
                  "  {"
                  "    i = i + 1;"
                  "  }"
                  "  return i;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, PrePostWithoutLoopStillWorks) {
  // Regression: make sure non-loop programs still verify correctly.
  auto v = verify("fn simple(a: i32, b: i32) -> i32"
                  "  pre { a > 0; b > 0; }"
                  "  post { r: r > a; r > b; }"
                  "{"
                  "  return a + b;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, DivisionWithPrecondition) {
  auto v = verify("fn safe_div(a: i32, b: i32) -> i32"
                  "  pre { a >= 0; b > 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  return a / b;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, MultiplicationBounds) {
  auto v = verify("fn mul_bounded(a: i32, b: i32) -> i32"
                  "  pre { a > 0; a < 10; b > 0; b < 10; }"
                  "  post { r: r > 0; r < 100; }"
                  "{"
                  "  return a * b;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, BooleanReturnContract) {
  auto v = verify("fn always_true() -> bool"
                  "  post { r: r == true; }"
                  "{"
                  "  return true;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, NestedIfElseVerifies) {
  auto v = verify("fn nested_branch(a: i32, b: i32) -> i32"
                  "  pre { a >= 0; b >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  if (a > b) {"
                  "    if (a > 10) {"
                  "      return a;"
                  "    } else {"
                  "      return b;"
                  "    }"
                  "  } else {"
                  "    return b;"
                  "  }"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, VariableAssignmentPreservesContract) {
  auto v = verify("fn assign(x: i32) -> i32"
                  "  pre { x > 0; }"
                  "  post { r: r > 1; }"
                  "{"
                  "  var y: i32 = x + 1;"
                  "  return y;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, MultipleVariablesInContract) {
  auto v = verify("fn multi_var(a: i32, b: i32) -> i32"
                  "  pre { a >= 0; b >= 0; a >= b; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  return a - b;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

// ===========================================================================
// Demonstrating what invariants unlock vs. what fails without them
// ===========================================================================

TEST(Verify, InvariantEnablesPostconditionProof) {
  // Same program, WITH invariant: verifies.
  auto with_inv = verify("fn with_inv(n: i32) -> i32"
                         "  pre { n >= 0; }"
                         "  post { r: r >= 0; }"
                         "{"
                         "  var acc: i32 = 0;"
                         "  var i: i32 = 0;"
                         "  while (i < n)"
                         "    invariant { acc >= 0; i >= 0; i <= n; }"
                         "  {"
                         "    acc = acc + i;"
                         "    i = i + 1;"
                         "  }"
                         "  return acc;"
                         "}");
  EXPECT_TRUE(isVerified(with_inv))
      << "With invariant, postcondition should be provable";

  // Same program, WITHOUT invariant: solver cannot prove postcondition.
  auto without_inv = verify("fn without_inv(n: i32) -> i32"
                            "  pre { n >= 0; }"
                            "  post { r: r >= 0; }"
                            "{"
                            "  var acc: i32 = 0;"
                            "  var i: i32 = 0;"
                            "  while (i < n) {"
                            "    acc = acc + i;"
                            "    i = i + 1;"
                            "  }"
                            "  return acc;"
                            "}");
  EXPECT_TRUE(isDisproven(without_inv))
      << "Without invariant, solver cannot prove postcondition about loops";
}

TEST(Verify, InvariantMakesCountdownProvable) {
  auto with_inv = verify("fn with_inv(n: i32) -> i32"
                         "  pre { n >= 0; }"
                         "  post { r: r == 0; }"
                         "{"
                         "  var i: i32 = n;"
                         "  while (i > 0)"
                         "    invariant { i >= 0; }"
                         "  {"
                         "    i = i - 1;"
                         "  }"
                         "  return i;"
                         "}");
  EXPECT_TRUE(isVerified(with_inv));

  auto without_inv = verify("fn without_inv(n: i32) -> i32"
                            "  pre { n >= 0; }"
                            "  post { r: r == 0; }"
                            "{"
                            "  var i: i32 = n;"
                            "  while (i > 0) {"
                            "    i = i - 1;"
                            "  }"
                            "  return i;"
                            "}");
  EXPECT_TRUE(isDisproven(without_inv));
}

// ===========================================================================
// Counter-example quality (structural checks on counterexamples)
// ===========================================================================

TEST(Verify, CounterExampleProvidedOnFailure) {
  auto v = verify("fn bad(x: i32) -> i32"
                  "  post { r: r > 100; }"
                  "{"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isDisproven(v));
  EXPECT_FALSE(v.result.counterExamples.empty())
      << "Disproven result should include at least one counter-example";
}

TEST(Verify, CounterExampleHasParameterValues) {
  auto v = verify("fn bad(x: i32) -> i32"
                  "  pre { x >= 0; }"
                  "  post { r: r > 100; }"
                  "{"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isDisproven(v));
  ASSERT_FALSE(v.result.counterExamples.empty());
  EXPECT_FALSE(v.result.counterExamples[0].parameters.empty())
      << "Counter-example should include parameter values";
}

// ===========================================================================
// Recursive function contracts
// ===========================================================================

TEST(Verify, RecursiveFunctionContract) {
  auto v = verify("fn fib(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  if (n <= 0) {"
                  "    return 0;"
                  "  }"
                  "  if (n == 1) {"
                  "    return 1;"
                  "  }"
                  "  return fib(n - 1) + fib(n - 2);"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

// ===========================================================================
// Interaction of call-site contracts and loop invariants
// ===========================================================================

TEST(Verify, InvariantWithFunctionCallInBody) {
  auto v = verify("fn inc(x: i32) -> i32"
                  "  pre { x >= 0; }"
                  "  post { r: r == x + 1; }"
                  "{"
                  "  return x + 1;"
                  "}"
                  "fn loop_with_call(n: i32) -> i32"
                  "  pre { n >= 0; }"
                  "  post { r: r >= 0; }"
                  "{"
                  "  var acc: i32 = 0;"
                  "  var i: i32 = 0;"
                  "  while (i < n)"
                  "    invariant { acc >= 0; i >= 0; i <= n; }"
                  "  {"
                  "    acc = inc(acc);"
                  "    i = i + 1;"
                  "  }"
                  "  return acc;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

// ===========================================================================
// Stress: larger programs to exercise the full pipeline
// ===========================================================================

TEST(Verify, BoundedDecrementFullVerification) {
  auto v = verify("fn bounded_dec(n: i32) -> i32"
                  "  pre { n >= 0; n < 1000; }"
                  "  post { r: r == 0; }"
                  "{"
                  "  var x: i32 = n;"
                  "  while (x > 0)"
                  "    invariant { x >= 0; }"
                  "  {"
                  "    x = x - 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, DoubleLoopMonotonicity) {
  auto v = verify("fn double_loop(n: i32) -> i32"
                  "  pre { n >= 1; n < 50; }"
                  "  post { r: r >= n; }"
                  "{"
                  "  var x: i32 = n;"
                  "  var i: i32 = 0;"
                  "  while (i < 3)"
                  "    invariant { x >= n; i >= 0; i <= 3; }"
                  "  {"
                  "    x = x + n;"
                  "    i = i + 1;"
                  "  }"
                  "  return x;"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

TEST(Verify, DeepCallChainVerifies) {
  auto v = verify("fn h(x: i32) -> i32"
                  "  pre { x > 0; }"
                  "  post { r: r > 0; }"
                  "{"
                  "  return x;"
                  "}"
                  "fn g(x: i32) -> i32"
                  "  pre { x > 0; }"
                  "  post { r: r > 0; }"
                  "{"
                  "  return h(x);"
                  "}"
                  "fn f(x: i32) -> i32"
                  "  pre { x > 0; }"
                  "  post { r: r > 0; }"
                  "{"
                  "  return g(x);"
                  "}"
                  "fn top() -> i32 {"
                  "  return f(42);"
                  "}");
  EXPECT_TRUE(isVerified(v));
}

} // namespace blaze::verification::tests
