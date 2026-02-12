#include "IrPrinter.h"
#include "Ast.h"
#include "Ir.h"
#include <variant>

std::string blaze::frontend::irPrint(const IRFunction &func) {
  std::string output;
  output += fmt::format("fn {}(", func.name);
  for (std::size_t i = 0; i < func.parameters.size(); ++i) {
    if (i > 0)
      output += ", ";
    const auto &param = func.parameters[i];
    output += fmt::format("{} {}", irPrint(param.reg), param.name);
  }
  output += fmt::format(") -> {}\n", irTypeToString(func.returnType));
  for (const auto &block : func.blocks) {
    output += irPrint(block);
  }
  return output;
}

std::string blaze::frontend::irPrint(const IRBlock &block) {
  std::string output;
  output += fmt::format(">{0}\n", block.id.id);
  for (const auto &instr : block.instructions) {
    output += fmt::format("  {0}\n", irPrint(instr));
  }
  if (block.terminator.has_value()) {
    output += fmt::format("  {0}\n", irPrint(*block.terminator));
  }
  return output;
}

std::string blaze::frontend::irPrint(const Terminator &term) {
  if (std::holds_alternative<ReturnTerminator>(term)) {
    const auto &ret = std::get<ReturnTerminator>(term);
    if (ret.returnValue.has_value()) {
      return fmt::format("ret {0}", irPrint(*ret.returnValue));
    }
    return "ret void";
  } else if (std::holds_alternative<BranchTerminator>(term)) {
    const auto &br = std::get<BranchTerminator>(term);
    return fmt::format("br {0} ? >{1} : >{2}", irPrint(Operand{br.condition}),
                       br.consequent.id, br.alternative.id);
  } else if (std::holds_alternative<JumpTerminator>(term)) {
    const auto &jmp = std::get<JumpTerminator>(term);
    return fmt::format("jmp >{0}", jmp.target.id);
  }

  return "<unknown terminator>";
}

std::string blaze::frontend::irPrint(const Instruction &instr) {
  std::string output;

  if (instr.isGhost) {
    output += "[ghost] ";
  }

  if (std::holds_alternative<BinaryInstruction>(instr)) {
    const auto &binaryInstr = std::get<BinaryInstruction>(instr);
    const auto dest = irPrint(binaryInstr.dest);
    const auto op = binaryOperationToString(binaryInstr.op);
    const auto &lhs = irPrint(binaryInstr.lhs);
    const auto &rhs = irPrint(binaryInstr.rhs);
    output += fmt::format("{0} = {1} {2} {3}", dest, op, lhs, rhs);
  } else if (std::holds_alternative<UnaryInstruction>(instr)) {
    const auto &unaryInstr = std::get<UnaryInstruction>(instr);
    const auto dest = irPrint(unaryInstr.dest);
    const auto &src = irPrint(unaryInstr.src);
    output += fmt::format("{0} = ! {1} ", dest, src);
  } else if (std::holds_alternative<CallInstruction>(instr)) {
    const auto &callInstr = std::get<CallInstruction>(instr);
    const auto dest = irPrint(callInstr.dest);
    const auto &callee = callInstr.func.name;
    const auto args = irPrint(callInstr.args);
    output += fmt::format("{0} = call {1}({2})", dest, callee, args);
  } else if (std::holds_alternative<AssignmentInstruction>(instr)) {
    const auto &assignInstr = std::get<AssignmentInstruction>(instr);
    const auto dest = irPrint(assignInstr.dest);
    const auto &src = irPrint(assignInstr.src);
    output += fmt::format("{0} = {1}", dest, src);
  } else if (std::holds_alternative<PhiInstruction>(instr)) {
    const auto &phiInstr = std::get<PhiInstruction>(instr);
    const auto dest = irPrint(phiInstr.dest);
    const auto &sources = irPrint(phiInstr.sources);
    output += fmt::format("{0} = phi {1}", dest, sources);
  } else if (std::holds_alternative<ContractCheckInstruction>(instr)) {
    const auto &contractInstr = std::get<ContractCheckInstruction>(instr);
    const auto &condition = irPrint(contractInstr.src);
    output += fmt::format("check {0}", condition);
  } else if (std::holds_alternative<ContractAssumeInstruction>(instr)) {
    const auto &contractInstr = std::get<ContractAssumeInstruction>(instr);
    const auto &condition = irPrint(contractInstr.src);
    output += fmt::format("assume {0}", condition);
  }

  return output;
}

std::string blaze::frontend::irPrint(const std::vector<Operand> &operands) {
  std::string output = " ";
  for (const auto &operand : operands) {
    output += fmt::format("{0} ", irPrint(operand));
  }
  return output;
}

std::string blaze::frontend::irPrint(const Register &reg) {
  return fmt::format("%{}:{}", reg.id, irTypeToString(reg.type));
}

std::string blaze::frontend::irPrint(const Operand &operand) {
  if (std::holds_alternative<Register>(operand)) {
    return irPrint(std::get<Register>(operand));
  } else if (std::holds_alternative<IntConstant>(operand)) {
    const auto &constant = std::get<IntConstant>(operand);
    return fmt::format("{0}", static_cast<unsigned long long>(constant.value));
  } else if (std::holds_alternative<BoolConstant>(operand)) {
    const auto &constant = std::get<BoolConstant>(operand);
    return fmt::format("{0}", constant.value ? "true" : "false");
  }

  return "<error>";
}

std::string blaze::frontend::irPrint(
    const std::vector<std::pair<BlockId, Register>> &phiBlocks) {
  std::string output;
  for (const auto &[blockId, reg] : phiBlocks) {
    output += fmt::format("[>{} {}] ", blockId.id, irPrint(reg));
  }
  return output;
}
