#include <filesystem>
#include <gtest/gtest.h>
#include <sstream>
#include <string>

#include "backend/llvm/llvmbackend.h"
#include "core/Errors.h"
#include "core/Source.h"
#include "frontend/Builtins.h"
#include "frontend/FrontendDriver.h"
#include "frontend/Ir.h"
#include "frontend/IrLower.h"
#include "frontend/SymbolTable.h"

#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

namespace blaze::backend::tests {

namespace {

struct LlvmIrOutcome {
  bool ok = false;
  core::DiagnosticList diagnostics;
  std::vector<frontend::IRFunction> functions;
  std::unique_ptr<backend::LLVMBackend> backend;
};

LlvmIrOutcome build(const std::string &code,
                    const std::string &filename = "llvm_ir_test.blz") {
  LlvmIrOutcome outcome;
  auto source = std::make_shared<core::Source>(code, filename);

  auto checked = frontend::checkSource(source, outcome.diagnostics);
  if (!checked) {
    outcome.ok = false;
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
    return outcome;
  }

  outcome.backend = std::make_unique<backend::LLVMBackend>(outcome.diagnostics);

  auto status = outcome.backend->generate(outcome.functions);
  outcome.ok = status == backend::CodeBackend::Success &&
               !outcome.diagnostics.hasErrors();
  return outcome;
}

/// Verify every non-declaration function in the module.
/// Returns an empty string on success, or the concatenated error messages.
std::string verifyAllFunctions(const llvm::Module *module) {
  std::string errors;
  for (const auto &fn : module->functions()) {
    if (fn.isDeclaration())
      continue;
    std::string fnErrors;
    llvm::raw_string_ostream rso(fnErrors);
    if (llvm::verifyFunction(fn, &rso)) {
      rso.flush();
      errors += fnErrors;
    }
  }
  return errors;
}

/// Dump the whole module's LLVM IR to a string for inspection.
std::string dumpModule(const llvm::Module *module) {
  std::string ir;
  llvm::raw_string_ostream rso(ir);
  module->print(rso, nullptr);
  rso.flush();
  return ir;
}

bool hasMemoryOps(const llvm::Function *fn) {
  for (const auto &inst : llvm::instructions(fn)) {
    if (llvm::isa<llvm::AllocaInst>(inst) || llvm::isa<llvm::LoadInst>(inst) ||
        llvm::isa<llvm::StoreInst>(inst)) {
      return true;
    }
  }
  return false;
}

} // namespace

// ---------------------------------------------------------------------------
// Basic function structure
// ---------------------------------------------------------------------------

TEST(LLVMBackend, EmitsSimpleReturnFunction) {
  auto outcome = build("fn foo() -> i32 { return 42; }");
  ASSERT_TRUE(outcome.ok);

  auto *module = outcome.backend->module();
  ASSERT_NE(module, nullptr);

  auto *fn = module->getFunction("foo");
  ASSERT_NE(fn, nullptr);
  EXPECT_FALSE(fn->empty());
  EXPECT_NE(fn->getEntryBlock().getTerminator(), nullptr);
  EXPECT_TRUE(fn->getReturnType()->isIntegerTy(32));
  EXPECT_EQ(fn->arg_size(), 0u);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, VoidFunctionHasVoidReturn) {
  auto outcome = build("fn noop() {}");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("noop");
  ASSERT_NE(fn, nullptr);
  EXPECT_TRUE(fn->getReturnType()->isVoidTy());
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, VoidFunctionWithExplicitReturn) {
  auto outcome = build("fn noop() { return; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("noop");
  ASSERT_NE(fn, nullptr);
  EXPECT_TRUE(fn->getReturnType()->isVoidTy());
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, BoolReturnFunction) {
  auto outcome = build("fn yes() -> bool { return true; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("yes");
  ASSERT_NE(fn, nullptr);
  EXPECT_TRUE(fn->getReturnType()->isIntegerTy(1));
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, FunctionReturnsFalse) {
  auto outcome = build("fn no() -> bool { return false; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("no");
  ASSERT_NE(fn, nullptr);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Function signatures / parameters
// ---------------------------------------------------------------------------

TEST(LLVMBackend, SingleI32Parameter) {
  auto outcome = build("fn id(x: i32) -> i32 { return x; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("id");
  ASSERT_NE(fn, nullptr);
  EXPECT_EQ(fn->arg_size(), 1u);
  EXPECT_TRUE(fn->getArg(0)->getType()->isIntegerTy(32));
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, MultipleMixedParameters) {
  auto outcome =
      build("fn f(a: i32, b: bool, c: i32) -> i32 { return a + c; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  EXPECT_EQ(fn->arg_size(), 3u);
  EXPECT_TRUE(fn->getArg(0)->getType()->isIntegerTy(32));
  EXPECT_TRUE(fn->getArg(1)->getType()->isIntegerTy(1));
  EXPECT_TRUE(fn->getArg(2)->getType()->isIntegerTy(32));
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, BoolParameter) {
  auto outcome = build("fn f(b: bool) -> bool { return b; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  EXPECT_EQ(fn->arg_size(), 1u);
  EXPECT_TRUE(fn->getArg(0)->getType()->isIntegerTy(1));
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, ZeroParameterVoidFunction) {
  auto outcome = build("fn f() { 42; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  EXPECT_EQ(fn->arg_size(), 0u);
  EXPECT_TRUE(fn->getReturnType()->isVoidTy());
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Arithmetic operations
// ---------------------------------------------------------------------------

TEST(LLVMBackend, Addition) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 { return a + b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, Subtraction) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 { return a - b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, Multiplication) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 { return a * b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, Division) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 { return a / b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ChainedArithmetic) {
  auto outcome = build(
      "fn f(a: i32, b: i32, c: i32) -> i32 { return (a + b) * c - a / b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ArithmeticWithConstants) {
  auto outcome = build("fn f() -> i32 { return 10 + 20 * 3; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, SubtractFromSelf) {
  auto outcome = build("fn f(x: i32) -> i32 { return x - x; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Comparison operations
// ---------------------------------------------------------------------------

TEST(LLVMBackend, LessThan) {
  auto outcome = build("fn f(a: i32, b: i32) -> bool { return a < b; }");
  ASSERT_TRUE(outcome.ok);
  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  EXPECT_TRUE(fn->getReturnType()->isIntegerTy(1));
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, LessEqual) {
  auto outcome = build("fn f(a: i32, b: i32) -> bool { return a <= b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, GreaterThan) {
  auto outcome = build("fn f(a: i32, b: i32) -> bool { return a > b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, GreaterEqual) {
  auto outcome = build("fn f(a: i32, b: i32) -> bool { return a >= b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, Equal) {
  auto outcome = build("fn f(a: i32, b: i32) -> bool { return a == b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, NotEqual) {
  auto outcome = build("fn f(a: i32, b: i32) -> bool { return a != b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, BooleanEquality) {
  auto outcome = build("fn f(a: bool, b: bool) -> bool { return a == b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ComparisonUsedInArithmetic) {
  // Comparison result stored in a bool variable, then used in a branch.
  auto outcome = build("fn f(x: i32) -> i32 {"
                       "  var positive: bool = x > 0;"
                       "  if (positive) { return x; }"
                       "  return 0;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Boolean / Unary NOT
// ---------------------------------------------------------------------------

TEST(LLVMBackend, EmitsBooleanNot) {
  auto outcome = build("fn foo(b: bool) -> bool { return !b; }");
  ASSERT_TRUE(outcome.ok);

  auto *module = outcome.backend->module();
  ASSERT_NE(module, nullptr);

  auto *fn = module->getFunction("foo");
  ASSERT_NE(fn, nullptr);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, DoubleNegation) {
  auto outcome = build("fn f(b: bool) -> bool { return !!b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, TripleNegation) {
  auto outcome = build("fn f(b: bool) -> bool { return !!!b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Variables / SSA / Assignment
// ---------------------------------------------------------------------------

TEST(LLVMBackend, SSALoweringAvoidsMemoryOps) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 {"
                       "  var x: i32 = a + b;"
                       "  if (x > 0) { x = x + 1; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
  EXPECT_FALSE(hasMemoryOps(fn));
}

TEST(LLVMBackend, SimpleVariable) {
  auto outcome = build("fn f() -> i32 { var x: i32 = 42; return x; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ConstVariable) {
  auto outcome = build("fn f() -> i32 { const x: i32 = 99; return x; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, VariableReassignment) {
  auto outcome =
      build("fn f() -> i32 { var x: i32 = 1; x = 2; x = 3; return x; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, SelfAccumulation) {
  auto outcome = build("fn f() -> i32 {"
                       "  var x: i32 = 1;"
                       "  x = x + 1;"
                       "  x = x + 1;"
                       "  x = x + 1;"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ManyVariables) {
  auto outcome = build("fn f() -> i32 {"
                       "  var a: i32 = 1;"
                       "  var b: i32 = 2;"
                       "  var c: i32 = 3;"
                       "  var d: i32 = 4;"
                       "  var e: i32 = 5;"
                       "  return a + b + c + d + e;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, VariableInitFromExpression) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 {"
                       "  var c: i32 = a * b + 1;"
                       "  return c;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, BoolVariable) {
  auto outcome =
      build("fn f() -> bool { var flag: bool = true; return flag; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, BoolVariableReassign) {
  auto outcome = build(
      "fn f() -> bool { var flag: bool = true; flag = false; return flag; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, VariableFromComparison) {
  auto outcome = build("fn f(a: i32, b: i32) -> bool {"
                       "  var result: bool = a < b;"
                       "  return result;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, DependencyChain) {
  auto outcome = build("fn f() -> i32 {"
                       "  var a: i32 = 1;"
                       "  var b: i32 = a + 1;"
                       "  var c: i32 = b + 1;"
                       "  var d: i32 = c + 1;"
                       "  return d;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Control flow: if / if-else
// ---------------------------------------------------------------------------

TEST(LLVMBackend, EmitsBranchingFunction) {
  auto outcome =
      build("fn foo(c: bool) -> i32 { if (c) { return 1; } return 2; }");
  ASSERT_TRUE(outcome.ok);

  auto *module = outcome.backend->module();
  ASSERT_NE(module, nullptr);

  auto *fn = module->getFunction("foo");
  ASSERT_NE(fn, nullptr);
  EXPECT_GE(fn->size(), 2u);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, IfElseBothReturn) {
  auto outcome = build("fn f(c: bool) -> i32 {"
                       "  if (c) { return 1; } else { return 2; }"
                       "}");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  // Both branches return — no merge block needed.
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, IfElseAssignAndMerge) {
  auto outcome = build("fn f(c: bool) -> i32 {"
                       "  var x: i32 = 0;"
                       "  if (c) { x = 1; } else { x = 2; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  // Should have entry + then + else + merge = 4 blocks.
  EXPECT_GE(fn->size(), 3u);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, IfWithoutElse) {
  auto outcome = build("fn f(c: bool) -> i32 {"
                       "  var x: i32 = 0;"
                       "  if (c) { x = 1; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, IfWithoutElseVoid) {
  auto outcome = build("fn f(flag: bool) { if (flag) { 42; } }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, IfElseIfElseChain) {
  auto outcome = build("fn f(x: i32) -> i32 {"
                       "  if (x > 0) { return 1; }"
                       "  else if (x < 0) { return 0 - 1; }"
                       "  else { return 0; }"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, DeeplyNestedIfElse) {
  auto outcome = build("fn f(a: i32, b: i32, c: i32) -> i32 {"
                       "  if (a > 0) {"
                       "    if (b > 0) {"
                       "      if (c > 0) { return 1; } else { return 2; }"
                       "    } else { return 3; }"
                       "  } else { return 4; }"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, SequentialIfs) {
  auto outcome = build("fn f(x: i32) -> i32 {"
                       "  var result: i32 = x;"
                       "  if (x > 10) { result = result - 10; }"
                       "  if (result > 5) { result = result - 5; }"
                       "  return result;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, IfWithLiteralTrueCondition) {
  auto outcome =
      build("fn f() -> i32 { if (true) { return 1; } else { return 0; } }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, IfWithLiteralFalseCondition) {
  auto outcome =
      build("fn f() -> i32 { if (false) { return 1; } else { return 0; } }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, NegatedBoolCondition) {
  auto outcome = build("fn f(flag: bool) -> i32 {"
                       "  if (!flag) { return 1; } else { return 0; }"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, EarlyReturnInThenBranch) {
  auto outcome = build("fn f(x: i32) -> i32 {"
                       "  if (x > 100) { return 100; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Control flow: while loops
// ---------------------------------------------------------------------------

TEST(LLVMBackend, SimpleWhileLoop) {
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var i: i32 = n;"
                       "  while (i > 0) { i = i - 1; }"
                       "  return i;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, WhileAccumulator) {
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var acc: i32 = 0;"
                       "  var i: i32 = 0;"
                       "  while (i < n) {"
                       "    acc = acc + i;"
                       "    i = i + 1;"
                       "  }"
                       "  return acc;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, WhileFalseNeverExecutes) {
  auto outcome = build(
      "fn f() -> i32 { var x: i32 = 0; while (false) { x = 99; } return x; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, WhileTrueWithEarlyReturn) {
  auto outcome =
      build("fn f() -> i32 { while (true) { return 7; } return 0; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, NestedWhileLoops) {
  auto outcome = build("fn f(rows: i32, cols: i32) -> i32 {"
                       "  var total: i32 = 0;"
                       "  var r: i32 = 0;"
                       "  while (r < rows) {"
                       "    var c: i32 = 0;"
                       "    while (c < cols) {"
                       "      total = total + 1;"
                       "      c = c + 1;"
                       "    }"
                       "    r = r + 1;"
                       "  }"
                       "  return total;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, WhileWithIfElseBody) {
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var a: i32 = 0;"
                       "  var b: i32 = 0;"
                       "  var i: i32 = 0;"
                       "  while (i < n) {"
                       "    if (i > 5) { a = a + 1; } else { b = b + 1; }"
                       "    i = i + 1;"
                       "  }"
                       "  return a + b;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, SequentialWhileLoops) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 {"
                       "  var x: i32 = a;"
                       "  while (x > 0) { x = x - 1; }"
                       "  var y: i32 = b;"
                       "  while (y > 0) { y = y - 1; }"
                       "  return x + y;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, WhileWithNegatedBoolCondition) {
  auto outcome = build("fn f(flag: bool) -> i32 {"
                       "  var count: i32 = 0;"
                       "  while (!flag) { count = count + 1; return count; }"
                       "  return count;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Function calls
// ---------------------------------------------------------------------------

TEST(LLVMBackend, SimpleFunctionCall) {
  auto outcome = build("fn double(x: i32) -> i32 { return x * 2; }"
                       "fn f() -> i32 { return double(5); }");
  ASSERT_TRUE(outcome.ok);

  auto *module = outcome.backend->module();
  auto errors = verifyAllFunctions(module);
  EXPECT_TRUE(errors.empty()) << errors;

  auto *fn = module->getFunction("double");
  ASSERT_NE(fn, nullptr);
  EXPECT_FALSE(fn->isDeclaration());
}

TEST(LLVMBackend, CallWithVariable) {
  auto outcome = build("fn double(x: i32) -> i32 { return x * 2; }"
                       "fn f() -> i32 { var n: i32 = 7; return double(n); }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, MultiArgumentCall) {
  auto outcome = build("fn add(a: i32, b: i32) -> i32 { return a + b; }"
                       "fn f() -> i32 { return add(3, 4); }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, NestedCalls) {
  auto outcome = build("fn double(x: i32) -> i32 { return x * 2; }"
                       "fn f(x: i32) -> i32 { return double(double(x)); }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, CallResultStoredInVariable) {
  auto outcome = build("fn double(x: i32) -> i32 { return x * 2; }"
                       "fn triple(x: i32) -> i32 { return x * 3; }"
                       "fn f(x: i32) -> i32 {"
                       "  var d: i32 = double(x);"
                       "  var t: i32 = triple(x);"
                       "  return d + t;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, VoidFunctionCallingVoidFunction) {
  auto outcome = build("fn inner(x: i32) { x + 1; }"
                       "fn outer(x: i32) { inner(x); }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, BoolReturningFunctionCall) {
  auto outcome = build("fn is_pos(x: i32) -> bool { return x > 0; }"
                       "fn f(x: i32) -> i32 {"
                       "  if (is_pos(x)) { return x; } else { return 0; }"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, CallInWhileCondition) {
  auto outcome = build("fn above(x: i32) -> bool { return x > 10; }"
                       "fn f(start: i32) -> i32 {"
                       "  var x: i32 = start;"
                       "  while (above(x)) { x = x - 1; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, CallWithExpressionArguments) {
  auto outcome =
      build("fn add(a: i32, b: i32) -> i32 { return a + b; }"
            "fn f(x: i32, y: i32) -> i32 { return add(x + 1, y * 2); }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, CallChain) {
  auto outcome =
      build("fn step1(x: i32) -> i32 { return x + 1; }"
            "fn step2(x: i32) -> i32 { return x * 2; }"
            "fn step3(x: i32) -> i32 { return x - 3; }"
            "fn f(x: i32) -> i32 { return step3(step2(step1(x))); }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

// ---------------------------------------------------------------------------
// Recursion
// ---------------------------------------------------------------------------

TEST(LLVMBackend, SimpleRecursion) {
  auto outcome = build("fn countdown(n: i32) -> i32 {"
                       "  if (n <= 0) { return 0; }"
                       "  return countdown(n - 1) + 1;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("countdown"), &llvm::errs()));
}

TEST(LLVMBackend, Factorial) {
  auto outcome = build("fn factorial(n: i32) -> i32 {"
                       "  if (n <= 1) { return 1; }"
                       "  return n * factorial(n - 1);"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("factorial"), &llvm::errs()));
}

TEST(LLVMBackend, Fibonacci) {
  auto outcome = build("fn fib(n: i32) -> i32 {"
                       "  if (n <= 0) { return 0; }"
                       "  if (n == 1) { return 1; }"
                       "  return fib(n - 1) + fib(n - 2);"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("fib"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Contracts are no-ops for codegen
// ---------------------------------------------------------------------------

TEST(LLVMBackend, PreconditionIsNoOp) {
  auto outcome = build("fn f(x: i32) -> i32"
                       "  pre { x > 0; }"
                       "{ return x; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, PostconditionIsNoOp) {
  auto outcome = build("fn f(x: i32) -> i32"
                       "  post { r: r > 0; }"
                       "{ return x; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, PreAndPostAreNoOps) {
  auto outcome = build("fn f(x: i32) -> i32"
                       "  pre { x > 0; }"
                       "  post { r: r >= x; }"
                       "{ return x + 1; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ContractCallSiteChecksAreNoOps) {
  auto outcome = build("fn contracted(x: i32) -> i32"
                       "  pre { x > 0; }"
                       "  post { r: r > 0; }"
                       "{ return x; }"
                       "fn f(x: i32) -> i32 { return contracted(x); }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, VoidFunctionWithPrePost) {
  auto outcome = build("fn f(x: i32)"
                       "  pre { x >= 0; }"
                       "  post { x >= 0; }"
                       "{ if (x == 0) { return; } x + 1; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ManyPreconditions) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32"
                       "  pre { a > 0; b > 0; a < 100; b < 100; }"
                       "{ return a + b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ManyPostconditions) {
  auto outcome = build("fn f(x: i32) -> i32"
                       "  post { r: r > 0; r < 1000; r >= 1; r != 0; }"
                       "{ return x + 1; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// While loops with invariants compile cleanly
// ---------------------------------------------------------------------------

TEST(LLVMBackend, WhileLoopWithInvariantCompilesCleanly) {
  // Loop invariants cause PHI instructions and contract assume/check
  // instructions to be emitted in the IR.  The codegen backend must
  // produce valid LLVM IR for the whole construct.
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var i: i32 = 0;"
                       "  while (i < n)"
                       "    invariant { i >= 0; }"
                       "  {"
                       "    i = i + 1;"
                       "  }"
                       "  return i;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, NestedLoopInvariantsVerify) {
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var outer: i32 = 0;"
                       "  var i: i32 = 0;"
                       "  while (i < n)"
                       "    invariant { i >= 0; }"
                       "  {"
                       "    var j: i32 = 0;"
                       "    while (j < n)"
                       "      invariant { j >= 0; }"
                       "    {"
                       "      outer = outer + 1;"
                       "      j = j + 1;"
                       "    }"
                       "    i = i + 1;"
                       "  }"
                       "  return outer;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Multiple functions in one module
// ---------------------------------------------------------------------------

TEST(LLVMBackend, MultipleFunctionsInModule) {
  auto outcome = build("fn a() -> i32 { return 1; }"
                       "fn b() -> i32 { return 2; }"
                       "fn c() -> i32 { return a() + b(); }");
  ASSERT_TRUE(outcome.ok);

  auto *module = outcome.backend->module();
  ASSERT_NE(module->getFunction("a"), nullptr);
  ASSERT_NE(module->getFunction("b"), nullptr);
  ASSERT_NE(module->getFunction("c"), nullptr);
  EXPECT_TRUE(verifyAllFunctions(module).empty());
}

TEST(LLVMBackend, ManyFunctions) {
  auto outcome = build("fn f1(x: i32) -> i32 { return x; }"
                       "fn f2(x: i32) -> i32 { return f1(x) + 1; }"
                       "fn f3(x: i32) -> i32 { return f2(x) + 1; }"
                       "fn f4(x: i32) -> i32 { return f3(x) + 1; }"
                       "fn f5(x: i32) -> i32 { return f4(x) + 1; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

// ---------------------------------------------------------------------------
// Blocks / scoping
// ---------------------------------------------------------------------------

TEST(LLVMBackend, NestedBlocks) {
  auto outcome = build("fn f() -> i32 {"
                       "  var x: i32 = 1;"
                       "  { var y: i32 = 2; x = x + y; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, DeeplyNestedBlocks) {
  auto outcome = build("fn f() -> i32 {"
                       "  var x: i32 = 0;"
                       "  { x = x + 1;"
                       "    { x = x + 1;"
                       "      { x = x + 1;"
                       "        { x = x + 1; }"
                       "      }"
                       "    }"
                       "  }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, SequentialBlocks) {
  auto outcome = build("fn f() -> i32 {"
                       "  var total: i32 = 0;"
                       "  { var a: i32 = 10; total = total + a; }"
                       "  { var b: i32 = 20; total = total + b; }"
                       "  return total;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Edge cases
// ---------------------------------------------------------------------------

TEST(LLVMBackend, EmptyVoidBody) {
  auto outcome = build("fn f() {}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ExpressionStatementDiscarded) {
  auto outcome = build("fn f(a: i32, b: i32) { a + b; a * b; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ReturnFromNestedBlock) {
  auto outcome = build("fn f() -> i32 { { return 42; } }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, BigConstant) {
  auto outcome = build("fn f() -> i32 { return 999999999; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ReturnZero) {
  auto outcome = build("fn f() -> i32 { return 0; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, MultipleVoidReturns) {
  auto outcome = build("fn f(flag: bool) { if (flag) { return; } return; }");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, SwapPattern) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32 {"
                       "  var x: i32 = a;"
                       "  var y: i32 = b;"
                       "  var temp: i32 = x;"
                       "  x = y;"
                       "  y = temp;"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Complex combinations
// ---------------------------------------------------------------------------

TEST(LLVMBackend, IfInsideWhileWithCalls) {
  auto outcome = build("fn helper(x: i32) -> i32"
                       "  pre { x > 0; }"
                       "  post { r: r > 0; }"
                       "{ return x; }"
                       "fn is_pos(x: i32) -> bool { return x > 0; }"
                       "fn f(n: i32) -> i32 {"
                       "  var acc: i32 = 0;"
                       "  var i: i32 = 1;"
                       "  while (i <= n) {"
                       "    if (is_pos(i)) {"
                       "      acc = acc + helper(i);"
                       "    } else {"
                       "      acc = acc + 1;"
                       "    }"
                       "    i = i + 1;"
                       "  }"
                       "  return acc;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, LoopWithMultipleReturnSites) {
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var i: i32 = 0;"
                       "  while (i < n) {"
                       "    if (i > 50) { return i; }"
                       "    i = i + 1;"
                       "  }"
                       "  return i;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, ContractedRecursiveFunction) {
  auto outcome = build("fn rec(n: i32) -> i32"
                       "  pre { n >= 0; }"
                       "  post { r: r == 0; }"
                       "{"
                       "  if (n == 0) { return 0; }"
                       "  return rec(n - 1);"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("rec"), &llvm::errs()));
}

TEST(LLVMBackend, ChainedContractedCalls) {
  auto outcome = build("fn h(x: i32) -> i32"
                       "  pre { x > 0; }"
                       "  post { r: r > 0; }"
                       "{ return x; }"
                       "fn f(x: i32) -> i32 {"
                       "  var a: i32 = h(x);"
                       "  var b: i32 = h(a);"
                       "  var c: i32 = h(b);"
                       "  return c;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_TRUE(verifyAllFunctions(outcome.backend->module()).empty());
}

TEST(LLVMBackend, DeepNestedControlFlowWithContracts) {
  auto outcome = build("fn f(a: i32, b: i32) -> i32"
                       "  post { r: r >= 0; }"
                       "{"
                       "  if (a > 0) {"
                       "    if (b > 0) { return a + b; }"
                       "    else { return a; }"
                       "  } else {"
                       "    if (b > 0) { return b; }"
                       "    else { return 0; }"
                       "  }"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, TripleNestedWhile) {
  auto outcome = build("fn f(a: i32, b: i32, c: i32) -> i32 {"
                       "  var count: i32 = 0;"
                       "  var i: i32 = 0;"
                       "  while (i < a) {"
                       "    var j: i32 = 0;"
                       "    while (j < b) {"
                       "      var k: i32 = 0;"
                       "      while (k < c) {"
                       "        count = count + 1;"
                       "        k = k + 1;"
                       "      }"
                       "      j = j + 1;"
                       "    }"
                       "    i = i + 1;"
                       "  }"
                       "  return count;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// LLVM IR output / module dump
// ---------------------------------------------------------------------------

TEST(LLVMBackend, ModuleDumpContainsFunctionName) {
  auto outcome = build("fn my_func() -> i32 { return 42; }");
  ASSERT_TRUE(outcome.ok);

  auto ir = dumpModule(outcome.backend->module());
  EXPECT_NE(ir.find("my_func"), std::string::npos)
      << "IR dump should contain the function name 'my_func'";
}

TEST(LLVMBackend, ModuleDumpContainsRetInstruction) {
  auto outcome = build("fn f() -> i32 { return 42; }");
  ASSERT_TRUE(outcome.ok);

  auto ir = dumpModule(outcome.backend->module());
  EXPECT_NE(ir.find("ret i32"), std::string::npos)
      << "IR dump should contain 'ret i32' for an i32-returning function";
}

TEST(LLVMBackend, ModuleDumpContainsVoidRet) {
  auto outcome = build("fn f() {}");
  ASSERT_TRUE(outcome.ok);

  auto ir = dumpModule(outcome.backend->module());
  EXPECT_NE(ir.find("ret void"), std::string::npos)
      << "IR dump should contain 'ret void' for a void function";
}

TEST(LLVMBackend, EmitIRToFileSmokeTest) {
  auto outcome = build("fn f() -> i32 { return 1; }");
  ASSERT_TRUE(outcome.ok);

  auto tmpPath =
      std::filesystem::temp_directory_path() / "blaze_test_emit_ir.ll";
  std::string error;
  bool ok = outcome.backend->emitIRToFile(tmpPath.string(), &error);
  EXPECT_TRUE(ok) << error;

  // Cleanup.
  std::filesystem::remove(tmpPath);
}

TEST(LLVMBackend, EmitIRToFileFailsWithoutModule) {
  core::DiagnosticList diagnostics;
  backend::LLVMBackend backend(diagnostics);

  std::string error;
  bool ok = backend.emitIRToFile("/dev/null", &error);
  EXPECT_FALSE(ok);
  EXPECT_FALSE(error.empty());
}

// ---------------------------------------------------------------------------
// Phi-like merge behaviour (variable assigned in both branches, read after)
// ---------------------------------------------------------------------------

TEST(LLVMBackend, PhiMergeBothBranchesAssign) {
  auto outcome = build("fn f(flag: bool) -> i32 {"
                       "  var x: i32 = 0;"
                       "  if (flag) { x = 10; } else { x = 20; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, PhiMergeThenBranchOnlyAssigns) {
  auto outcome = build("fn f(flag: bool) -> i32 {"
                       "  var x: i32 = 5;"
                       "  if (flag) { x = 99; }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, PhiMergeNestedIfElse) {
  auto outcome = build("fn f(a: bool, b: bool) -> i32 {"
                       "  var x: i32 = 0;"
                       "  if (a) {"
                       "    if (b) { x = 1; } else { x = 2; }"
                       "  } else {"
                       "    x = 3;"
                       "  }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

TEST(LLVMBackend, PhiMergeWhileLoop) {
  // Variables modified inside a while loop merge correctly at the loop header.
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var x: i32 = 0;"
                       "  var i: i32 = 0;"
                       "  while (i < n) {"
                       "    x = x + i;"
                       "    i = i + 1;"
                       "  }"
                       "  return x;"
                       "}");
  ASSERT_TRUE(outcome.ok);
  EXPECT_FALSE(llvm::verifyFunction(
      *outcome.backend->module()->getFunction("f"), &llvm::errs()));
}

// ---------------------------------------------------------------------------
// Block count / structure assertions
// ---------------------------------------------------------------------------

TEST(LLVMBackend, StraightLineFunctionSingleBlock) {
  auto outcome = build("fn f(x: i32) -> i32 { return x + 1; }");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  // A straight-line function should have exactly one basic block.
  EXPECT_EQ(fn->size(), 1u);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, IfElseBothReturnNoMergeBlock) {
  auto outcome = build("fn f(c: bool) -> i32 {"
                       "  if (c) { return 1; } else { return 2; }"
                       "}");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  // entry (with branch) + then-block + else-block = 3
  // No merge block because both branches return.
  EXPECT_EQ(fn->size(), 3u);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

TEST(LLVMBackend, WhileLoopBlockStructure) {
  auto outcome = build("fn f(n: i32) -> i32 {"
                       "  var i: i32 = n;"
                       "  while (i > 0) { i = i - 1; }"
                       "  return i;"
                       "}");
  ASSERT_TRUE(outcome.ok);

  auto *fn = outcome.backend->module()->getFunction("f");
  ASSERT_NE(fn, nullptr);
  // At minimum: entry, header, body, exit
  EXPECT_GE(fn->size(), 3u);
  EXPECT_FALSE(llvm::verifyFunction(*fn, &llvm::errs()));
}

// ---------------------------------------------------------------------------
// generate() error path
// ---------------------------------------------------------------------------

TEST(LLVMBackend, GenerateReturnsSuccessForValidCode) {
  core::DiagnosticList diagnostics;
  auto source =
      std::make_shared<core::Source>("fn f() -> i32 { return 1; }", "test.blz");
  auto checked = frontend::checkSource(source, diagnostics);
  ASSERT_TRUE(checked.has_value());

  auto symbolTable =
      std::make_shared<frontend::SymbolTable>(checked->resolve.symbols);
  auto builtins =
      std::make_shared<frontend::BuiltinRegistry>(checked->resolve.builtins);
  frontend::IRBuilder builder(diagnostics);
  auto functions = builder.lower(checked->parse.root, symbolTable, builtins);
  ASSERT_FALSE(diagnostics.hasErrors());

  backend::LLVMBackend backend(diagnostics);
  auto status = backend.generate(functions);
  EXPECT_EQ(status, backend::CodeBackend::Success);
}

// ---------------------------------------------------------------------------
// Bulk: compile all test files and verify LLVM IR
// ---------------------------------------------------------------------------

TEST(LLVMBackend, CompilesAllTestfiles) {
  namespace fs = std::filesystem;
  const fs::path testfilesDir =
      fs::absolute(fs::path(__FILE__)).parent_path() / "testfiles";
  ASSERT_TRUE(fs::exists(testfilesDir))
      << "Missing testfiles dir: " << testfilesDir.string();

  std::vector<std::string> failures;
  auto diagnosticsToString =
      [](const core::DiagnosticList &diagnostics) -> std::string {
    std::ostringstream oss;
    for (const auto &diag : diagnostics) {
      if (diag.sourceLocation.line() != 0) {
        oss << diag.sourceLocation.line() << ":" << diag.sourceLocation.column()
            << ": ";
      }
      oss << diag.errorCode << " " << diag.message << "\n";
    }
    return oss.str();
  };

  for (const auto &entry : fs::directory_iterator(testfilesDir)) {
    if (!entry.is_regular_file())
      continue;
    if (entry.path().extension() != ".blz")
      continue;

    core::DiagnosticList diagnostics;
    auto checked = frontend::checkFile(entry.path().string(), diagnostics);
    if (!checked) {
      failures.push_back(entry.path().filename().string() + ": check failed\n" +
                         diagnosticsToString(diagnostics));
      continue;
    }

    auto symbolTable =
        std::make_shared<frontend::SymbolTable>(checked->resolve.symbols);
    auto builtins =
        std::make_shared<frontend::BuiltinRegistry>(checked->resolve.builtins);

    frontend::IRBuilder builder(diagnostics);
    auto functions = builder.lower(checked->parse.root, symbolTable, builtins);
    if (diagnostics.hasErrors()) {
      failures.push_back(entry.path().filename().string() +
                         ": lowering errors\n" +
                         diagnosticsToString(diagnostics));
      continue;
    }

    backend::LLVMBackend backend(diagnostics);
    auto status = backend.generate(functions);
    if (status != backend::CodeBackend::Success || diagnostics.hasErrors() ||
        !backend.module()) {
      failures.push_back(entry.path().filename().string() +
                         ": backend errors\n" +
                         diagnosticsToString(diagnostics));
      continue;
    }

    bool invalid = false;
    std::string verifyOutput;
    for (auto &fn : backend.module()->functions()) {
      if (fn.isDeclaration())
        continue;
      std::string fnErrors;
      llvm::raw_string_ostream rso(fnErrors);
      if (llvm::verifyFunction(fn, &rso)) {
        rso.flush();
        verifyOutput += fnErrors;
        invalid = true;
      }
    }

    if (invalid)
      failures.push_back(entry.path().filename().string() +
                         ": llvm verification failed\n" + verifyOutput);
  }

  if (!failures.empty()) {
    std::ostringstream oss;
    for (size_t i = 0; i < failures.size(); ++i) {
      if (i > 0)
        oss << "\n---\n";
      oss << failures[i];
    }
    FAIL() << "LLVM IR generation failed:\n" << oss.str();
  }
}

} // namespace blaze::backend::tests
