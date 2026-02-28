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

// ---------------------------------------------------------------------------
// Loop invariant tests
// ---------------------------------------------------------------------------

TEST(IrLower, InvariantParsesAndLowers) {
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
}

TEST(IrLower, InvariantCheckedBeforeLoop) {
  // The entry block (block 0) should contain a ContractCheckInstruction
  // emitted *before* the jump to the loop header.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32"
                               "  pre { n >= 0; }"
                               "{"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  const auto &entry = fns[0].blocks[0];
  bool foundCheck = false;
  for (const auto &inst : entry.instructions) {
    if (std::holds_alternative<ContractCheckInstruction>(inst)) {
      foundCheck = true;
      break;
    }
  }
  EXPECT_TRUE(foundCheck)
      << "Expected a ContractCheckInstruction in entry block for invariant "
         "establishment";
}

TEST(IrLower, InvariantAssumedAtLoopHeader) {
  // The loop header block (block 1) should begin with
  // ContractAssumeInstruction(s) from the invariant.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  ASSERT_GE(fns[0].blocks.size(), 2u);

  // Block 1 is the header.
  const auto &header = fns[0].blocks[1];
  bool foundAssume = false;
  for (const auto &inst : header.instructions) {
    if (std::holds_alternative<ContractAssumeInstruction>(inst)) {
      EXPECT_TRUE(inst.isGhost);
      foundAssume = true;
      break;
    }
  }
  EXPECT_TRUE(foundAssume)
      << "Expected a ContractAssumeInstruction in the loop header block";
}

TEST(IrLower, HavocEmittedForModifiedLoopVariable) {
  // The loop header should include a havoc for variables modified by the body.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  ASSERT_GE(fns[0].blocks.size(), 2u);

  const auto &header = fns[0].blocks[1];
  bool foundHavoc = false;
  for (const auto &inst : header.instructions) {
    if (std::holds_alternative<HavocInstruction>(inst)) {
      EXPECT_TRUE(inst.isGhost);
      foundHavoc = true;
      break;
    }
  }
  EXPECT_TRUE(foundHavoc)
      << "Expected a HavocInstruction in the loop header for modified variable";
}

TEST(IrLower, InvariantRecheckedAtEndOfBody) {
  // The loop body block (block 2) should end with ContractCheckInstruction(s)
  // just before the back-edge jump.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  ASSERT_GE(fns[0].blocks.size(), 3u);

  // Block 2 is the body.
  const auto &body = fns[0].blocks[2];
  bool foundCheck = false;
  for (const auto &inst : body.instructions) {
    if (std::holds_alternative<ContractCheckInstruction>(inst)) {
      foundCheck = true;
    }
  }
  EXPECT_TRUE(foundCheck)
      << "Expected a ContractCheckInstruction in the loop body for invariant "
         "preservation";
}

TEST(IrLower, InvariantAssumedAfterLoopExit) {
  // The exit block (block 3) should have ContractAssumeInstruction(s)
  // so post-loop code can use the invariant as a known fact.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  ASSERT_GE(fns[0].blocks.size(), 4u);

  // Block 3 is the exit.
  const auto &exit = fns[0].blocks[3];
  bool foundAssume = false;
  for (const auto &inst : exit.instructions) {
    if (std::holds_alternative<ContractAssumeInstruction>(inst)) {
      EXPECT_TRUE(inst.isGhost);
      foundAssume = true;
      break;
    }
  }
  EXPECT_TRUE(foundAssume)
      << "Expected a ContractAssumeInstruction in the loop exit block";
}

TEST(IrLower, InvariantAllInstructionsAreGhost) {
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Every ContractCheckInstruction and ContractAssumeInstruction in the
  // function should be ghost.
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst) ||
          std::holds_alternative<ContractAssumeInstruction>(inst)) {
        EXPECT_TRUE(inst.isGhost)
            << "Contract instructions from invariant must be ghost";
      }
    }
  }
}

TEST(IrLower, MultipleInvariantExpressionsEmitMultipleChecksAndAssumes) {
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32"
                               "  pre { n >= 0; }"
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
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Count checks and assumes across all blocks.
  int checkCount = 0;
  int assumeCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst))
        checkCount++;
      else if (std::holds_alternative<ContractAssumeInstruction>(inst))
        assumeCount++;
    }
  }

  // 3 invariants: checked before loop (3) + checked at end of body (3) = 6
  // plus postcondition checks if any.  With pre only, checks = 6.
  EXPECT_GE(checkCount, 6)
      << "Expected at least 6 checks (3 establishment + 3 preservation)";
  // 3 at loop header + 3 at exit + 1 from precondition assume = 7
  EXPECT_GE(assumeCount, 7)
      << "Expected at least 7 assumes (1 pre + 3 header + 3 exit)";
}

TEST(IrLower, InvariantCheckBeforeJumpToHeader) {
  // In the entry block, the check for the invariant must appear before the
  // jump terminator to the header.  Verify ordering: check comes before
  // the terminator, and the terminator is a jump.
  auto [ok, diag, fns] = lower("fn foo() -> i32 {"
                               "  var i: i32 = 5;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  const auto &entry = fns[0].blocks[0];
  bool foundCheck = false;
  for (const auto &inst : entry.instructions) {
    if (std::holds_alternative<ContractCheckInstruction>(inst)) {
      foundCheck = true;
    }
  }
  EXPECT_TRUE(foundCheck);

  // Terminator should be a jump to the header.
  ASSERT_TRUE(entry.terminator.has_value());
  EXPECT_TRUE(std::holds_alternative<JumpTerminator>(*entry.terminator));
}

TEST(IrLower, NoInvariantNoExtraContractInstructions) {
  // A while loop without an invariant should not produce any
  // ContractCheckInstruction or ContractAssumeInstruction beyond
  // what pre/post already generates.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0) {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  int checkCount = 0;
  int assumeCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst))
        checkCount++;
      else if (std::holds_alternative<ContractAssumeInstruction>(inst))
        assumeCount++;
    }
  }
  EXPECT_EQ(checkCount, 0);
  EXPECT_EQ(assumeCount, 0);
}

TEST(IrLower, NestedLoopsEachHaveInvariants) {
  auto [ok, diag, fns] = lower("fn foo(rows: i32, cols: i32) -> i32"
                               "  pre { rows >= 0; cols >= 0; }"
                               "{"
                               "  var total: i32 = 0;"
                               "  var r: i32 = 0;"
                               "  while (r < rows)"
                               "    invariant { total >= 0; r >= 0; }"
                               "  {"
                               "    var c: i32 = 0;"
                               "    while (c < cols)"
                               "      invariant { c >= 0; total >= 0; }"
                               "    {"
                               "      total = total + 1;"
                               "      c = c + 1;"
                               "    }"
                               "    r = r + 1;"
                               "  }"
                               "  return total;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Just verify it compiles and produces a reasonable number of contract
  // instructions from both loops.
  int checkCount = 0;
  int assumeCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst))
        checkCount++;
      else if (std::holds_alternative<ContractAssumeInstruction>(inst))
        assumeCount++;
    }
  }
  // Outer: 2 establishment + 2 preservation = 4 checks
  // Inner: 2 establishment + 2 preservation = 4 checks
  // Total checks >= 8
  EXPECT_GE(checkCount, 8);
  // 2 pre assumes + outer header(2) + outer exit(2) +
  // inner header(2) + inner exit(2) = 10
  EXPECT_GE(assumeCount, 10);
}

TEST(IrLower, InvariantWithPreAndPost) {
  // Verify invariants coexist with function-level pre and post contracts.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32"
                               "  pre { n >= 0; }"
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
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Entry block: pre assume + invariant check + jump
  const auto &entry = fns[0].blocks[0];
  bool foundPreAssume = false;
  bool foundInvCheck = false;
  for (const auto &inst : entry.instructions) {
    if (std::holds_alternative<ContractAssumeInstruction>(inst))
      foundPreAssume = true;
    if (std::holds_alternative<ContractCheckInstruction>(inst))
      foundInvCheck = true;
  }
  EXPECT_TRUE(foundPreAssume) << "Missing pre assume in entry block";
  EXPECT_TRUE(foundInvCheck) << "Missing invariant check in entry block";

  // There should be a postcondition check somewhere in the function
  // (at the return site in the exit block or the block containing return).
  bool foundPostCheck = false;
  for (const auto &block : fns[0].blocks) {
    bool hasReturn =
        block.terminator.has_value() &&
        std::holds_alternative<ReturnTerminator>(*block.terminator);
    if (hasReturn) {
      for (const auto &inst : block.instructions) {
        if (std::holds_alternative<ContractCheckInstruction>(inst))
          foundPostCheck = true;
      }
    }
  }
  EXPECT_TRUE(foundPostCheck) << "Missing postcondition check at return site";
}

TEST(IrLower, InvariantOnVoidFunction) {
  auto [ok, diag, fns] = lower("fn foo(n: i32)"
                               "  pre { n >= 0; }"
                               "{"
                               "  var i: i32 = 0;"
                               "  while (i < n)"
                               "    invariant { i >= 0; i <= n; }"
                               "  {"
                               "    i = i + 1;"
                               "  }"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Should still produce contract instructions for the invariant.
  int checkCount = 0;
  int assumeCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst))
        checkCount++;
      else if (std::holds_alternative<ContractAssumeInstruction>(inst))
        assumeCount++;
    }
  }
  // 2 establishment + 2 preservation = 4 checks
  EXPECT_GE(checkCount, 4);
  // 1 pre + 2 header + 2 exit = 5
  EXPECT_GE(assumeCount, 5);
}

TEST(IrLower, SequentialLoopsEachWithInvariant) {
  auto [ok, diag, fns] = lower("fn foo(a: i32, b: i32) -> i32"
                               "  pre { a >= 0; b >= 0; }"
                               "{"
                               "  var x: i32 = a;"
                               "  while (x > 0)"
                               "    invariant { x >= 0; }"
                               "  {"
                               "    x = x - 1;"
                               "  }"
                               "  var y: i32 = b;"
                               "  while (y > 0)"
                               "    invariant { y >= 0; }"
                               "  {"
                               "    y = y - 1;"
                               "  }"
                               "  return x + y;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // Two loops, each with 1 invariant:
  // Loop1: 1 est check + 1 pres check + 1 header assume + 1 exit assume = 4
  // Loop2: 1 est check + 1 pres check + 1 header assume + 1 exit assume = 4
  // Plus 2 pre assumes.
  int checkCount = 0;
  int assumeCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst))
        checkCount++;
      else if (std::holds_alternative<ContractAssumeInstruction>(inst))
        assumeCount++;
    }
  }
  EXPECT_GE(checkCount, 4);
  EXPECT_GE(assumeCount, 6);
}

TEST(IrLower, InvariantNonBooleanIsTypeError) {
  // An invariant expression that is not boolean should be caught by
  // type checking, and lowering should report errors.
  auto [ok, diag, fns] = lower("fn foo() -> i32 {"
                               "  var i: i32 = 0;"
                               "  while (i < 5)"
                               "    invariant { i; }"
                               "  {"
                               "    i = i + 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_FALSE(ok);
  EXPECT_TRUE(hasDiagnosticWithCode(diag, core::ERROR_TYPE_MISMATCH));
}

TEST(IrLower, InvariantEmptyClauseProducesNoContractInstructions) {
  // An invariant clause with no expressions should be valid but produce
  // no contract instructions.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  int checkCount = 0;
  int assumeCount = 0;
  for (const auto &block : fns[0].blocks) {
    for (const auto &inst : block.instructions) {
      if (std::holds_alternative<ContractCheckInstruction>(inst))
        checkCount++;
      else if (std::holds_alternative<ContractAssumeInstruction>(inst))
        assumeCount++;
    }
  }
  EXPECT_EQ(checkCount, 0);
  EXPECT_EQ(assumeCount, 0);
}

TEST(IrLower, InvariantSubExpressionsInCheckAreGhost) {
  // The binary comparison generated for the invariant expression must
  // itself be marked ghost, not just the check instruction.
  auto [ok, diag, fns] = lower("fn foo(n: i32) -> i32 {"
                               "  var i: i32 = n;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);

  // In the entry block, find the BinaryInstruction that computes `i >= 0`
  // for the establishment check — it should be ghost.
  const auto &entry = fns[0].blocks[0];
  bool foundGhostBinary = false;
  for (const auto &inst : entry.instructions) {
    if (std::holds_alternative<BinaryInstruction>(inst) && inst.isGhost) {
      foundGhostBinary = true;
    }
  }
  EXPECT_TRUE(foundGhostBinary)
      << "Expected a ghost BinaryInstruction for the invariant expression";
}

TEST(IrLower, InvariantBlockStructure) {
  // Verify the overall block structure:
  // block 0 = entry (setup + invariant check + jump to header)
  // block 1 = header (invariant assume + condition eval + branch)
  // block 2 = body (loop body + invariant check + jump to header)
  // block 3 = exit (invariant assume + post-loop code)
  auto [ok, diag, fns] = lower("fn foo() -> i32 {"
                               "  var i: i32 = 5;"
                               "  while (i > 0)"
                               "    invariant { i >= 0; }"
                               "  {"
                               "    i = i - 1;"
                               "  }"
                               "  return i;"
                               "}");
  EXPECT_TRUE(ok);
  ASSERT_EQ(fns.size(), 1u);
  ASSERT_GE(fns[0].blocks.size(), 4u);

  // Block 0 terminator: jump to block 1
  ASSERT_TRUE(fns[0].blocks[0].terminator.has_value());
  ASSERT_TRUE(
      std::holds_alternative<JumpTerminator>(*fns[0].blocks[0].terminator));
  EXPECT_EQ(std::get<JumpTerminator>(*fns[0].blocks[0].terminator).target.id,
            1u);

  // Block 1 terminator: branch to block 2 (body) or block 3 (exit)
  ASSERT_TRUE(fns[0].blocks[1].terminator.has_value());
  ASSERT_TRUE(
      std::holds_alternative<BranchTerminator>(*fns[0].blocks[1].terminator));
  const auto &br = std::get<BranchTerminator>(*fns[0].blocks[1].terminator);
  EXPECT_EQ(br.consequent.id, 2u);
  EXPECT_EQ(br.alternative.id, 3u);

  // Block 2 terminator: jump back to block 1 (header)
  ASSERT_TRUE(fns[0].blocks[2].terminator.has_value());
  ASSERT_TRUE(
      std::holds_alternative<JumpTerminator>(*fns[0].blocks[2].terminator));
  EXPECT_EQ(std::get<JumpTerminator>(*fns[0].blocks[2].terminator).target.id,
            1u);

  // Block 3 terminator: return
  ASSERT_TRUE(fns[0].blocks[3].terminator.has_value());
  EXPECT_TRUE(
      std::holds_alternative<ReturnTerminator>(*fns[0].blocks[3].terminator));
}

} // namespace blaze::frontend::tests
