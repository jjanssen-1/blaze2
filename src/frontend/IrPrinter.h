#pragma once

#include "frontend/Ir.h"

#include <fmt/format.h>
#include <string>

namespace blaze::frontend {

std::string irPrint(const std::vector<std::pair<BlockId, Register>> &phiBlocks);
std::string irPrint(const std::vector<Operand> &operands);
std::string irPrint(const Register &reg);
std::string irPrint(const Operand &operand);
std::string irPrint(const Instruction &instr);
std::string irPrint(const Terminator &term);
std::string irPrint(const IRBlock &block);
std::string irPrint(const IRFunction &func);

} // namespace blaze::frontend
