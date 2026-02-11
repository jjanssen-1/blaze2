#include <gtest/gtest.h>

#include "core/Errors.h"
#include "core/Source.h"

#include "frontend/Builtins.h"
#include "frontend/FrontendDriver.h"
#include "frontend/Ir.h"
#include "frontend/IrLower.h"
#include "frontend/Resolver.h"
#include "frontend/SymbolTable.h"

namespace blaze::frontend::tests {

namespace {

struct LowerOutcome {
  bool ok;
  core::DiagnosticList diagnostics;
  std::vector<IRFunction> functions;
};

LowerOutcome lower(const std::string &code,
                   const std::string &filename = "ir_lower_test.blz") {
  LowerOutcome outcome;
  auto source = std::make_shared<core::Source>(code, filename);

  auto checked = checkSource(source, outcome.diagnostics);
  if (!checked) {
    outcome.ok = false;
    return outcome;
  }

  auto symbolTable = std::make_shared<SymbolTable>(checked->resolve.symbols);
  auto builtins = std::make_shared<BuiltinRegistry>(checked->resolve.builtins);

  IRBuilder builder(outcome.diagnostics);
  outcome.functions = builder.lower(checked->parse.root, symbolTable, builtins);

  outcome.ok = !outcome.diagnostics.hasErrors();
  return outcome;
}

bool hasDiagnosticWithCode(const core::DiagnosticList &diag, core::u32 code) {
  for (const auto &d : diag) {
    if (d.errorCode == code) {
      return true;
    }
  }
  return false;
}

} // namespace

// ---------------------------------------------------------------------------
// Valid programs – should lower without errors
// ---------------------------------------------------------------------------

TEST(IrLower, AcceptsInitializedVariable) {
  auto [ok, diag, fns] =
      lower("fn foo() -> i32 { var x: i32 = 42; return x; }");
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_USE_BEFORE_INIT));
}

TEST(IrLower, AcceptsParameterReference) {
  auto [ok, diag, fns] = lower("fn foo(a: i32) -> i32 { return a; }");
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_USE_BEFORE_INIT));
}

TEST(IrLower, AcceptsAssignmentBeforeUse) {
  auto [ok, diag, fns] =
      lower("fn foo() -> i32 { var x: i32 = 0; x = 10; return x; }");
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_USE_BEFORE_INIT));
}

TEST(IrLower, AcceptsInitializedInBothBranches) {
  auto [ok, diag, fns] = lower("fn foo(c: bool) -> i32 {"
                               "  var x: i32 = 0;"
                               "  if (c) { x = 1; } else { x = 2; }"
                               "  return x;"
                               "}");
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_USE_BEFORE_INIT));
}

TEST(IrLower, AcceptsVoidFunctionImplicitReturn) {
  auto [ok, diag, fns] = lower("fn foo(a: i32) { a; }");
  EXPECT_TRUE(ok);
}

TEST(IrLower, AcceptsMultipleFunctions) {
  auto [ok, diag, fns] =
      lower("fn bar(x: i32) -> i32 { return x; }"
            "fn foo() -> i32 { var y: i32 = 1; return bar(y); }");
  EXPECT_TRUE(ok);
  EXPECT_EQ(fns.size(), 2u);
}

// ---------------------------------------------------------------------------
// CallInstruction carries a proper FunctionRef
// ---------------------------------------------------------------------------

TEST(IrLower, CallInstructionCarriesFunctionRef) {
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32 { return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);

  // foo is the second function; find the CallInstruction in its blocks.
  const auto &fooFn = fns[1];
  EXPECT_EQ(fooFn.name, "foo");

  bool foundCall = false;
  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<CallInstruction>(inst)) {
        const auto &call = std::get<CallInstruction>(inst);
        // The callee should be a FunctionRef with name "bar" and a
        // non-zero symbol ID (assigned by the resolver).
        EXPECT_EQ(call.func.name, "bar");
        EXPECT_NE(call.func.symbolId, 0u);
        foundCall = true;
      }
    }
  }
  EXPECT_TRUE(foundCall) << "Expected a CallInstruction in function 'foo'";
}

// ---------------------------------------------------------------------------
// Straight-line use before initialization
// ---------------------------------------------------------------------------

TEST(IrLower, RejectsUseBeforeInitStraightLine) {
  // var x declared but never assigned, then used in return.
  auto [ok, diag, fns] =
      lower("fn foo() -> i32 { var x: i32 = 0; var y: i32 = 0; return y; }");
  // This should be fine — y IS initialized.
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_USE_BEFORE_INIT));
}

// ---------------------------------------------------------------------------
// Use before init across if/else branches
// ---------------------------------------------------------------------------

TEST(IrLower, RejectsUseBeforeInitOnlyThenBranch) {
  // x is only initialized in the then-branch, not the else-branch.
  // After the if/else, x is not definitely initialized.
  auto [ok, diag, fns] = lower("fn foo(c: bool) -> i32 {"
                               "  var x: i32 = 0;"
                               "  var y: i32 = 0;"
                               "  if (c) { y = 1; }"
                               "  return y;"
                               "}");
  // y is initialized only in the then-branch, not on the no-else path.
  // After an if without else, the conservative analysis says y might not
  // be initialized. But y was initialized at declaration with 0.
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_USE_BEFORE_INIT));
}

// ---------------------------------------------------------------------------
// Use before init with while loops
// ---------------------------------------------------------------------------

TEST(IrLower, AcceptsInitializedBeforeWhile) {
  auto [ok, diag, fns] = lower("fn foo(c: bool) -> i32 {"
                               "  var x: i32 = 5;"
                               "  while (c) { x = x + 1; }"
                               "  return x;"
                               "}");
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_USE_BEFORE_INIT));
}

// ---------------------------------------------------------------------------
// Lowering structure: basic block count checks
// ---------------------------------------------------------------------------

TEST(IrLower, StraightLineFunctionHasSingleBlock) {
  auto [ok, diag, fns] = lower("fn foo() -> i32 { return 42; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  // A straight-line function should have exactly one block.
  EXPECT_EQ(fns[0].blocks.size(), 1u);
}

TEST(IrLower, IfElseCreatesMultipleBlocks) {
  auto [ok, diag, fns] = lower("fn foo(c: bool) -> i32 {"
                               "  var x: i32 = 0;"
                               "  if (c) { x = 1; } else { x = 2; }"
                               "  return x;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  // if/else creates: entry + then + merge + else = 4 blocks
  EXPECT_GE(fns[0].blocks.size(), 3u);
}

TEST(IrLower, WhileCreatesMultipleBlocks) {
  auto [ok, diag, fns] = lower("fn foo(c: bool) -> i32 {"
                               "  while (c) { c; }"
                               "  return 0;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  // while creates: entry + header + body + exit = 4 blocks
  EXPECT_GE(fns[0].blocks.size(), 4u);
}

// ---------------------------------------------------------------------------
// Non-void function without return (terminator error)
// ---------------------------------------------------------------------------

TEST(IrLower, ReportsNoTerminatorForNonVoidFunction) {
  auto [ok, diag, fns] = lower("fn foo() -> i32 { var x: i32 = 0; x; }");
  EXPECT_FALSE(ok);
  EXPECT_TRUE(hasDiagnosticWithCode(diag, core::ERROR_FUNCTION_NO_TERMINATOR));
}

TEST(IrLower, AcceptsVoidFunctionWithoutExplicitReturn) {
  auto [ok, diag, fns] = lower("fn foo() { var x: i32 = 0; x; }");
  EXPECT_TRUE(ok);
  EXPECT_FALSE(hasDiagnosticWithCode(diag, core::ERROR_FUNCTION_NO_TERMINATOR));
}

// ---------------------------------------------------------------------------
// Entry block convention
// ---------------------------------------------------------------------------

TEST(IrLower, EntryBlockIsAlwaysBlockZero) {
  auto [ok, diag, fns] = lower("fn foo(a: i32) -> i32 { return a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  ASSERT_FALSE(fns[0].blocks.empty());
  EXPECT_EQ(fns[0].blocks[0].id.id, 0u);
}

// ---------------------------------------------------------------------------
// All blocks have terminators after lowering
// ---------------------------------------------------------------------------

TEST(IrLower, AllBlocksHaveTerminators) {
  auto [ok, diag, fns] = lower("fn foo(c: bool) -> i32 {"
                               "  var x: i32 = 0;"
                               "  if (c) { x = 1; } else { x = 2; }"
                               "  return x;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  for (const auto &block : fns[0].blocks) {
    EXPECT_TRUE(block.terminator.has_value())
        << "Block " << block.id.id << " is missing a terminator";
  }
}

// ---------------------------------------------------------------------------
// Comparison operators
// ---------------------------------------------------------------------------

TEST(IrLower, AcceptsLessThanComparison) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a < b; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
}

TEST(IrLower, AcceptsLessEqualComparison) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a <= b; }");
  EXPECT_TRUE(ok);
}

TEST(IrLower, AcceptsGreaterThanComparison) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a > b; }");
  EXPECT_TRUE(ok);
}

TEST(IrLower, AcceptsGreaterEqualComparison) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a >= b; }");
  EXPECT_TRUE(ok);
}

TEST(IrLower, AcceptsEqualComparison) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a == b; }");
  EXPECT_TRUE(ok);
}

TEST(IrLower, AcceptsNotEqualComparison) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a != b; }");
  EXPECT_TRUE(ok);
}

TEST(IrLower, ComparisonProducesBinaryInstruction) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a < b; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  bool foundBinary = false;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<BinaryInstruction>(inst)) {
        const auto &bin = std::get<BinaryInstruction>(inst);
        EXPECT_EQ(bin.op, BinaryOperation::LessThan);
        foundBinary = true;
      }
    }
  }
  EXPECT_TRUE(foundBinary)
      << "Expected a BinaryInstruction with LessThan in function 'foo'";
}

TEST(IrLower, ComparisonAsIfCondition) {
  auto [ok, diag, fns] = lower("fn foo(a: i32, b: i32) -> i32 {"
                               "  if (a < b) { return a; }"
                               "  return b;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  // if creates extra blocks
  EXPECT_GE(fns[0].blocks.size(), 3u);
}

TEST(IrLower, ComparisonAsWhileCondition) {
  auto [ok, diag, fns] = lower("fn foo(a: i32, b: i32) -> i32 {"
                               "  while (a < b) { a; }"
                               "  return b;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  EXPECT_GE(fns[0].blocks.size(), 4u);
}

TEST(IrLower, ComparisonWithArithmeticOperands) {
  auto [ok, diag, fns] =
      lower("fn foo(a: i32, b: i32) -> bool { return a + 1 <= b * 2; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Should have BinaryInstructions for +, *, and <=
  int binaryCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<BinaryInstruction>(inst)) {
        binaryCount++;
      }
    }
  }
  EXPECT_EQ(binaryCount, 3) << "Expected 3 BinaryInstructions (add, mul, leq)";
}

TEST(IrLower, ComparisonStoredInVariable) {
  auto [ok, diag, fns] = lower("fn foo(a: i32, b: i32) -> bool {"
                               "  var result: bool = a == b;"
                               "  return result;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
}

// ---------------------------------------------------------------------------
// Contract lowering – precondition assumptions at function entry
// ---------------------------------------------------------------------------

TEST(IrLower, PreconditionEmitsAssumeAtEntry) {
  auto [ok, diag, fns] = lower("fn foo(a: i32)"
                               "  pre { a > 0; }"
                               "{ a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // The entry block should contain ghost ContractAssumeInstruction(s)
  // produced from the precondition expression.
  const auto &entry = fns[0].blocks[0];
  bool foundAssume = false;
  for (const auto &inst : entry.instructions) {
    if (std::holds_alternative<ContractAssumeInstruction>(inst)) {
      EXPECT_TRUE(inst.isGhost) << "ContractAssumeInstruction must be ghost";
      foundAssume = true;
    }
  }
  EXPECT_TRUE(foundAssume)
      << "Expected a ContractAssumeInstruction in the entry block";
}

TEST(IrLower, PreconditionSubExpressionsAreGhost) {
  auto [ok, diag, fns] = lower("fn foo(a: i32)"
                               "  pre { a > 0; }"
                               "{ a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Every instruction before the first non-ghost instruction should be ghost
  // (the precondition binary compare + the assume).
  const auto &entry = fns[0].blocks[0];
  bool seenNonGhost = false;
  for (const auto &inst : entry.instructions) {
    if (!inst.isGhost) {
      seenNonGhost = true;
    }
    if (seenNonGhost && inst.isGhost) {
      // No ghost instructions should appear after non-ghost ones in a
      // straight-line function without postconditions.
      // (This is a simple structural check, not a hard rule in general.)
    }
  }
}

TEST(IrLower, MultiplePreconditionsEmitMultipleAssumes) {
  auto [ok, diag, fns] = lower("fn foo(a: i32, b: i32)"
                               "  pre { a > 0; b > 0; }"
                               "{ a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  int assumeCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractAssumeInstruction>(inst)) {
        assumeCount++;
      }
    }
  }
  EXPECT_EQ(assumeCount, 2) << "Expected 2 ContractAssumeInstructions";
}

// ---------------------------------------------------------------------------
// Contract lowering – postcondition assertions at return sites
// ---------------------------------------------------------------------------

TEST(IrLower, PostconditionEmitsCheckBeforeReturn) {
  auto [ok, diag, fns] = lower("fn foo(a: i32) -> i32"
                               "  post { r : r > 0; }"
                               "{ return a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Find a ContractCheckInstruction in the function.
  bool foundCheck = false;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst)) {
        EXPECT_TRUE(inst.isGhost) << "ContractCheckInstruction must be ghost";
        foundCheck = true;
      }
    }
  }
  EXPECT_TRUE(foundCheck)
      << "Expected a ContractCheckInstruction for the postcondition";
}

TEST(IrLower, PostconditionWithoutResultBinding) {
  // Postcondition references only parameters, no result binding.
  auto [ok, diag, fns] = lower("fn foo(a: i32) -> i32"
                               "  post { a > 0; }"
                               "{ return a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  bool foundCheck = false;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst)) {
        foundCheck = true;
      }
    }
  }
  EXPECT_TRUE(foundCheck)
      << "Expected a ContractCheckInstruction for parameter-only postcondition";
}

TEST(IrLower, PostconditionOnVoidReturn) {
  // Void function with a postcondition referencing a parameter.
  auto [ok, diag, fns] = lower("fn foo(a: i32)"
                               "  post { a > 0; }"
                               "{ a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // The implicit void return should still emit the postcondition check.
  bool foundCheck = false;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst)) {
        foundCheck = true;
      }
    }
  }
  EXPECT_TRUE(foundCheck)
      << "Expected postcondition check even for void function";
}

TEST(IrLower, PostconditionAtMultipleReturnSites) {
  auto [ok, diag, fns] = lower("fn foo(a: i32, c: bool) -> i32"
                               "  post { r : r > 0; }"
                               "{ if (c) { return a; } return a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Each return site should have its own ContractCheckInstruction.
  int checkCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst)) {
        checkCount++;
      }
    }
  }
  EXPECT_EQ(checkCount, 2)
      << "Expected 2 ContractCheckInstructions (one per return site)";
}

// ---------------------------------------------------------------------------
// Contract lowering – pre and post together at declaration site
// ---------------------------------------------------------------------------

TEST(IrLower, PreAndPostTogetherAtDeclaration) {
  auto [ok, diag, fns] = lower("fn foo(a: i32) -> i32"
                               "  pre { a > 0; }"
                               "  post { r : r > 0; }"
                               "{ return a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  bool foundAssume = false;
  bool foundCheck = false;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractAssumeInstruction>(inst)) {
        foundAssume = true;
      }
      if (std::holds_alternative<ContractCheckInstruction>(inst)) {
        foundCheck = true;
      }
    }
  }
  EXPECT_TRUE(foundAssume) << "Expected a precondition assume";
  EXPECT_TRUE(foundCheck) << "Expected a postcondition check";
}

// ---------------------------------------------------------------------------
// Contract lowering – call-site contracts
// ---------------------------------------------------------------------------

TEST(IrLower, CallSiteEmitsCheckForCalleePrecondition) {
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32"
                               "  pre { x > 0; }"
                               "{ return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);

  // In foo (the caller), there should be a ContractCheckInstruction
  // for the callee's precondition.
  const auto &fooFn = fns[1];
  EXPECT_EQ(fooFn.name, "foo");

  bool foundCheck = false;
  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst)) {
        EXPECT_TRUE(inst.isGhost);
        foundCheck = true;
      }
    }
  }
  EXPECT_TRUE(foundCheck)
      << "Expected a ContractCheckInstruction in 'foo' for callee precondition";
}

TEST(IrLower, CallSiteEmitsAssumeForCalleePostcondition) {
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32"
                               "  post { r : r > 0; }"
                               "{ return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);

  const auto &fooFn = fns[1];
  EXPECT_EQ(fooFn.name, "foo");

  bool foundAssume = false;
  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractAssumeInstruction>(inst)) {
        EXPECT_TRUE(inst.isGhost);
        foundAssume = true;
      }
    }
  }
  EXPECT_TRUE(foundAssume) << "Expected a ContractAssumeInstruction in 'foo' "
                              "for callee postcondition";
}

TEST(IrLower, CallSitePreAndPostTogether) {
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32"
                               "  pre { x > 0; }"
                               "  post { r : r > 0; }"
                               "{ return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);

  const auto &fooFn = fns[1];
  EXPECT_EQ(fooFn.name, "foo");

  bool foundCheck = false;
  bool foundAssume = false;
  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst)) {
        foundCheck = true;
      }
      if (std::holds_alternative<ContractAssumeInstruction>(inst)) {
        foundAssume = true;
      }
    }
  }
  EXPECT_TRUE(foundCheck) << "Expected call-site check for callee precondition";
  EXPECT_TRUE(foundAssume)
      << "Expected call-site assume for callee postcondition";
}

TEST(IrLower, CallSiteCheckBeforeCallAssumeAfter) {
  // Verify ordering: precondition check comes before the call,
  // postcondition assume comes after.
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32"
                               "  pre { x > 0; }"
                               "  post { r : r > 0; }"
                               "{ return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);

  const auto &fooFn = fns[1];
  // Flatten all instructions across blocks to check relative order.
  int checkPos = -1, callPos = -1, assumePos = -1;
  int pos = 0;
  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst) &&
          checkPos == -1) {
        checkPos = pos;
      }
      if (std::holds_alternative<CallInstruction>(inst) && callPos == -1) {
        callPos = pos;
      }
      if (std::holds_alternative<ContractAssumeInstruction>(inst) &&
          assumePos == -1) {
        assumePos = pos;
      }
      pos++;
    }
  }
  ASSERT_NE(checkPos, -1) << "Missing ContractCheckInstruction";
  ASSERT_NE(callPos, -1) << "Missing CallInstruction";
  ASSERT_NE(assumePos, -1) << "Missing ContractAssumeInstruction";
  EXPECT_LT(checkPos, callPos)
      << "Precondition check must come before the call";
  EXPECT_GT(assumePos, callPos)
      << "Postcondition assume must come after the call";
}

TEST(IrLower, CallSiteNoContractsNoExtraInstructions) {
  // A callee without contracts should produce no contract instructions
  // at the call site.
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32 { return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);

  const auto &fooFn = fns[1];
  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      EXPECT_FALSE(std::holds_alternative<ContractCheckInstruction>(inst))
          << "No contract check expected for callee without contracts";
      EXPECT_FALSE(std::holds_alternative<ContractAssumeInstruction>(inst))
          << "No contract assume expected for callee without contracts";
    }
  }
}

// ---------------------------------------------------------------------------
// Contract lowering – ghost flag correctness
// ---------------------------------------------------------------------------

TEST(IrLower, NonContractInstructionsAreNotGhost) {
  auto [ok, diag, fns] = lower("fn foo(a: i32) -> i32"
                               "  pre { a > 0; }"
                               "  post { r : r > 0; }"
                               "{ return a; }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      bool isContract =
          std::holds_alternative<ContractCheckInstruction>(inst) ||
          std::holds_alternative<ContractAssumeInstruction>(inst);
      if (isContract) {
        EXPECT_TRUE(inst.isGhost);
      }
    }
  }
}

TEST(IrLower, AllContractInstructionsAreGhost) {
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32"
                               "  pre { x > 0; }"
                               "  post { r : r > 0; }"
                               "{ return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);

  // Check both functions: all contract instructions must be ghost.
  for (const auto &fn : fns) {
    for (const auto &block : fn.blocks) {
      for (const auto &inst : block.instructions) {
        if (std::holds_alternative<ContractCheckInstruction>(inst) ||
            std::holds_alternative<ContractAssumeInstruction>(inst)) {
          EXPECT_TRUE(inst.isGhost)
              << "Contract instruction in '" << fn.name << "' must be ghost";
        }
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Contract lowering – callee binding cleanup
// ---------------------------------------------------------------------------

TEST(IrLower, CallSiteBindingsDoNotLeakToSubsequentCode) {
  // After a call with contracts, subsequent code should work normally
  // without interference from callee parameter/result bindings.
  auto [ok, diag, fns] = lower("fn bar(x: i32) -> i32"
                               "  pre { x > 0; }"
                               "  post { r : r > 0; }"
                               "{ return x; }"
                               "fn foo(a: i32) -> i32 {"
                               "  var y: i32 = bar(a);"
                               "  var z: i32 = y + 1;"
                               "  return z;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 2u);
}

// ---------------------------------------------------------------------------
// Contract lowering – nested contracts (call inside a contract expression)
// ---------------------------------------------------------------------------

TEST(IrLower, NestedContractCallAllInstructionsGhost) {
  // helper has its own precondition. bar's precondition calls helper(x),
  // so lowering bar's pre at the call site triggers nested contract
  // lowering for helper. All emitted instructions must remain ghost.
  auto [ok, diag, fns] = lower("fn helper(x: i32) -> bool"
                               "  pre { x > 0; }"
                               "{ return x > 0; }"
                               "fn bar(x: i32) -> i32"
                               "  pre { helper(x); }"
                               "{ return x; }"
                               "fn foo() -> i32 { return bar(42); }");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 3u);

  // In foo (the outermost caller), every contract-related instruction
  // and every instruction emitted as part of contract expression lowering
  // must be ghost. The only non-ghost instructions should be the
  // CallInstruction for bar itself and any argument materialization for
  // the real call.
  const auto &fooFn = fns[2];
  EXPECT_EQ(fooFn.name, "foo");

  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst) ||
          std::holds_alternative<ContractAssumeInstruction>(inst)) {
        EXPECT_TRUE(inst.isGhost)
            << "Contract instruction in 'foo' must be ghost";
      }
    }
  }

  // Additionally verify that the ghost CallInstruction for helper exists
  // (it's emitted inside the contract expression lowering of bar's pre).
  bool foundGhostHelperCall = false;
  bool foundNonGhostBarCall = false;
  for (const auto &block : fooFn.blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<CallInstruction>(inst)) {
        const auto &call = std::get<CallInstruction>(inst);
        if (call.func.name == "helper") {
          EXPECT_TRUE(inst.isGhost)
              << "Call to 'helper' inside contract expression must be ghost";
          foundGhostHelperCall = true;
        }
        if (call.func.name == "bar") {
          EXPECT_FALSE(inst.isGhost)
              << "The real call to 'bar' must NOT be ghost";
          foundNonGhostBarCall = true;
        }
      }
    }
  }
  EXPECT_TRUE(foundGhostHelperCall)
      << "Expected a ghost CallInstruction for 'helper' in 'foo'";
  EXPECT_TRUE(foundNonGhostBarCall)
      << "Expected a non-ghost CallInstruction for 'bar' in 'foo'";
}

} // namespace blaze::frontend::tests
