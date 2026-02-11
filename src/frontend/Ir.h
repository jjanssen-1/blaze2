#pragma once

#include "Ast.h"
#include "core/Common.h"
#include "core/Source.h"
#include <optional>
#include <variant>
#include <vector>

namespace blaze::frontend {

// Registers and BlockIds are numbered locally per-function.
// Each function starts from Register{0} and BlockId{0}.
// The entry block of every function is always BlockId{0}.

struct Register {
  core::u64 id;
  inline bool operator==(const Register &other) const { return id == other.id; }
};

struct BlockId {
  core::u64 id;
  inline bool operator==(const BlockId &other) const { return id == other.id; }
};

struct IntConstant {
  core::u64 value;
};

struct BoolConstant {
  bool value;
};

typedef std::variant<Register, IntConstant, BoolConstant> Operand;

/// A reference to a named function. Used as the callee in CallInstruction
/// rather than a generic Operand, since static calls always target a known
/// function symbol. Carries both the human-readable name (useful for IR
/// printing/debugging) and the raw symbol ID for resolution.
struct FunctionRef {
  std::string name;
  core::u64 symbolId;

  inline bool operator==(const FunctionRef &other) const {
    return symbolId == other.symbolId;
  }
};

struct BinaryInstruction {
  BinaryOperation op;
  Register dest;
  Operand lhs, rhs;
};

struct UnaryInstruction {
  UnaryOperation op;
  Register dest;
  Operand src;
};

struct CallInstruction {
  Register dest;
  FunctionRef func;
  std::vector<Operand> args;
};

struct AssignmentInstruction {
  Register dest;
  Operand src;
};

struct ContractCheckInstruction {
  Operand src;
};

struct ContractAssumeInstruction {
  Operand src;
};

struct PhiInstruction {
  Register dest;
  std::vector<std::pair<BlockId, Register>> sources;
};

// Instruction inherits from the variant so that std::holds_alternative,
// std::get, and std::visit continue to work unchanged, while also carrying
// a SourceLocation and ghost flag for richer diagnostics.
struct Instruction
    : public std::variant<BinaryInstruction, UnaryInstruction, CallInstruction,
                          AssignmentInstruction, PhiInstruction,
                          ContractCheckInstruction, ContractAssumeInstruction> {
  using Variant =
      std::variant<BinaryInstruction, UnaryInstruction, CallInstruction,
                   AssignmentInstruction, PhiInstruction,
                   ContractCheckInstruction, ContractAssumeInstruction>;

  template <typename T, typename = std::enable_if_t<
                            !std::is_same_v<std::decay_t<T>, Instruction> &&
                            std::is_constructible_v<Variant, T &&>>>
  Instruction(const core::SourceLocation &loc, bool ghost, T &&val)
      : Variant(std::forward<T>(val)), location(loc), isGhost(ghost) {}

  const core::SourceLocation location;
  const bool isGhost;
};

struct ReturnTerminator {
  std::optional<Operand> returnValue;
};

struct BranchTerminator {
  Register condition;
  BlockId consequent;
  BlockId alternative;
};

struct JumpTerminator {
  BlockId target;
};

// Terminator inherits from the variant so that std::holds_alternative,
// std::get, and std::visit continue to work unchanged, while also carrying
// a SourceLocation for richer diagnostics.
struct Terminator
    : public std::variant<ReturnTerminator, BranchTerminator, JumpTerminator> {
  using Variant =
      std::variant<ReturnTerminator, BranchTerminator, JumpTerminator>;

  template <typename T, typename = std::enable_if_t<
                            !std::is_same_v<std::decay_t<T>, Terminator> &&
                            std::is_constructible_v<Variant, T &&>>>
  Terminator(const core::SourceLocation &loc, T &&val)
      : Variant(std::forward<T>(val)), location(loc) {}

  const core::SourceLocation location;
};

// A basic block: a straight-line sequence of instructions ending in
// an optional terminator. The terminator is std::nullopt while the
// block is still under construction and must be set before the
// function is considered complete.
struct IRBlock {
  BlockId id;
  std::vector<Instruction> instructions;
  std::optional<Terminator> terminator;
};

// An IR-level function. Block and register IDs are local to the function.
// The entry block is always the first block (blocks[0], id == BlockId{0}).
struct IRFunction {
  std::string name;
  std::vector<IRBlock> blocks;
};

} // namespace blaze::frontend
