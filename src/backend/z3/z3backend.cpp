#include "z3backend.h"

#include <map>
#include <set>
#include <variant>
#include <vector>

#include <z3++.h>

#include "frontend/Ir.h"

namespace {

using namespace blaze::frontend;
using namespace blaze::backend;
using namespace blaze::core;

struct ForwardEdge {
  u64 from;
  u64 to;
};

struct CheckInfo {
  z3::expr reachable;
  z3::expr condition;
  SourceLocation location;
};

struct FunctionVerifier {
  z3::context ctx;
  z3::solver solver;
  std::map<u64, z3::expr> regs;
  std::set<u64> paramRegIds;
  std::vector<ForwardEdge> forwardEdges;
  std::map<std::pair<u64, u64>, z3::expr> edgeVars;
  std::map<u64, z3::expr> blockReach;
  std::vector<CheckInfo> checks;

  FunctionVerifier() : solver(ctx) {}

  void createRegisterVariables(const IRFunction &function) {
    // register -> z3::expr
    auto makeRegVar = [&](const Register &reg) {
      if (reg.type == IRType::Void)
        return;
      if (regs.count(reg.id))
        return;
      std::string name = "r" + std::to_string(reg.id);
      switch (reg.type) {
      case IRType::I32:
        regs.emplace(reg.id, ctx.int_const(name.c_str()));
        break;
      case IRType::Bool:
        regs.emplace(reg.id, ctx.bool_const(name.c_str()));
        break;
      default:
        break;
      }
    };

    auto makeOperandVar = [&](const Operand &op) {
      if (std::holds_alternative<Register>(op))
        makeRegVar(std::get<Register>(op));
    };

    for (const auto &param : function.parameters) {
      makeRegVar(param.reg);
      paramRegIds.insert(param.reg.id);
    }

    for (const auto &block : function.blocks) {
      for (const auto &instr : block.instructions) {
        if (std::holds_alternative<BinaryInstruction>(instr)) {
          const auto &bi = std::get<BinaryInstruction>(instr);
          makeRegVar(bi.dest);
          makeOperandVar(bi.lhs);
          makeOperandVar(bi.rhs);
        } else if (std::holds_alternative<UnaryInstruction>(instr)) {
          const auto &ui = std::get<UnaryInstruction>(instr);
          makeRegVar(ui.dest);
          makeOperandVar(ui.src);
        } else if (std::holds_alternative<CallInstruction>(instr)) {
          const auto &ci = std::get<CallInstruction>(instr);
          makeRegVar(ci.dest);
          for (const auto &arg : ci.args)
            makeOperandVar(arg);
        } else if (std::holds_alternative<AssignmentInstruction>(instr)) {
          const auto &ai = std::get<AssignmentInstruction>(instr);
          makeRegVar(ai.dest);
          makeOperandVar(ai.src);
        } else if (std::holds_alternative<HavocInstruction>(instr)) {
          const auto &hi = std::get<HavocInstruction>(instr);
          makeRegVar(hi.dest);
        } else if (std::holds_alternative<PhiInstruction>(instr)) {
          const auto &pi = std::get<PhiInstruction>(instr);
          makeRegVar(pi.dest);
          for (const auto &[_, srcReg] : pi.sources)
            makeRegVar(srcReg);
        } else if (std::holds_alternative<ContractCheckInstruction>(instr)) {
          makeOperandVar(std::get<ContractCheckInstruction>(instr).src);
        } else if (std::holds_alternative<ContractAssumeInstruction>(instr)) {
          makeOperandVar(std::get<ContractAssumeInstruction>(instr).src);
        }
      }

      // Also scan terminators for register references.
      if (block.terminator.has_value()) {
        const auto &term = *block.terminator;
        if (std::holds_alternative<BranchTerminator>(term))
          makeRegVar(std::get<BranchTerminator>(term).condition);
        else if (std::holds_alternative<ReturnTerminator>(term)) {
          const auto &ret = std::get<ReturnTerminator>(term);
          if (ret.returnValue.has_value())
            makeOperandVar(*ret.returnValue);
        }
      }
    }
  }

  z3::expr operandToExpr(const Operand &op) {
    if (std::holds_alternative<Register>(op))
      return regs.at(std::get<Register>(op).id);
    if (std::holds_alternative<IntConstant>(op))
      return ctx.int_val(static_cast<int64_t>(std::get<IntConstant>(op).value));
    return ctx.bool_val(std::get<BoolConstant>(op).value);
  }

  void buildReachability(const IRFunction &function) {
    // Collect edges, skipping back edges (target id <= source id).
    // Back edges only arise from while-loop jumps back to the header.
    // Excluding them breaks the cycle so Z3 doesn't need to reason about
    // loop semantics directly; loop summaries are provided by invariant
    // checks/assumes in the IR (plus havoc), and loop-carried phi
    // destinations are left unconstrained, which soundly over-approximates
    // all possible iterations.
    for (const auto &block : function.blocks) {
      if (!block.terminator.has_value())
        continue;
      const auto &term = *block.terminator;
      if (std::holds_alternative<BranchTerminator>(term)) {
        const auto &br = std::get<BranchTerminator>(term);
        if (br.consequent.id > block.id.id)
          forwardEdges.push_back({block.id.id, br.consequent.id});
        if (br.alternative.id > block.id.id)
          forwardEdges.push_back({block.id.id, br.alternative.id});
      } else if (std::holds_alternative<JumpTerminator>(term)) {
        const auto &jmp = std::get<JumpTerminator>(term);
        if (jmp.target.id > block.id.id)
          forwardEdges.push_back({block.id.id, jmp.target.id});
      }
    }

    for (const auto &e : forwardEdges) {
      std::string name =
          "edge_" + std::to_string(e.from) + "_" + std::to_string(e.to);
      edgeVars.emplace(std::make_pair(e.from, e.to),
                       ctx.bool_const(name.c_str()));
    }

    blockReach.emplace(0, ctx.bool_val(true));
    for (const auto &block : function.blocks) {
      if (block.id.id == 0)
        continue;
      z3::expr_vector incoming(ctx);
      for (const auto &e : forwardEdges) {
        if (e.to == block.id.id)
          incoming.push_back(edgeVars.at({e.from, e.to}));
      }
      if (incoming.empty())
        blockReach.emplace(block.id.id, ctx.bool_val(false));
      else if (incoming.size() == 1)
        blockReach.emplace(block.id.id, incoming[0]);
      else
        blockReach.emplace(block.id.id, z3::mk_or(incoming));
    }
  }

  // Add edge constraints to the solver.
  void addEdgeConstraints(const IRFunction &function) {
    for (const auto &block : function.blocks) {
      if (!block.terminator.has_value())
        continue;
      const auto &term = *block.terminator;
      z3::expr reach = blockReach.at(block.id.id);

      if (std::holds_alternative<BranchTerminator>(term)) {
        const auto &br = std::get<BranchTerminator>(term);
        z3::expr cond = regs.at(br.condition.id);
        if (br.consequent.id > block.id.id)
          solver.add(edgeVars.at({block.id.id, br.consequent.id}) ==
                     (reach && cond));
        if (br.alternative.id > block.id.id)
          solver.add(edgeVars.at({block.id.id, br.alternative.id}) ==
                     (reach && !cond));
      } else if (std::holds_alternative<JumpTerminator>(term)) {
        const auto &jmp = std::get<JumpTerminator>(term);
        if (jmp.target.id > block.id.id)
          solver.add(edgeVars.at({block.id.id, jmp.target.id}) == reach);
      }
    }
  }

  void addBinaryConstraint(const z3::expr &reach, const BinaryInstruction &bi) {
    if (bi.dest.type == IRType::Void)
      return;
    z3::expr dest = regs.at(bi.dest.id);
    z3::expr lhs = operandToExpr(bi.lhs);
    z3::expr rhs = operandToExpr(bi.rhs);
    z3::expr result = [&]() -> z3::expr {
      switch (bi.op) {
      case Addition:
        return lhs + rhs;
      case Subtraction:
        return lhs - rhs;
      case Multiplication:
        return lhs * rhs;
      case Division:
        return lhs / rhs;
      case LessThan:
        return lhs < rhs;
      case LessEqual:
        return lhs <= rhs;
      case GreaterThan:
        return lhs > rhs;
      case GreaterEqual:
        return lhs >= rhs;
      case Equal:
        return lhs == rhs;
      case NotEqual:
        return lhs != rhs;
      default:
        return ctx.bool_val(false);
      }
    }();
    solver.add(z3::implies(reach, dest == result));
  }

  void addPhiConstraint(const z3::expr &reach, const PhiInstruction &pi,
                        u64 blockId) {
    if (pi.dest.type == IRType::Void || pi.sources.empty())
      return;

    // -> buildReachability
    bool hasBackEdge = false;
    for (const auto &[srcBlock, srcReg] : pi.sources) {
      if (srcBlock.id >= blockId) {
        hasBackEdge = true;
        break;
      }
    }
    if (hasBackEdge)
      return;

    z3::expr dest = regs.at(pi.dest.id);
    z3::expr result = regs.at(pi.sources.back().second.id);
    for (int i = static_cast<int>(pi.sources.size()) - 2; i >= 0; --i) {
      auto key = std::make_pair(pi.sources[i].first.id, blockId);
      auto it = edgeVars.find(key);
      if (it != edgeVars.end()) {
        z3::expr srcExpr = regs.at(pi.sources[i].second.id);
        result = z3::ite(it->second, srcExpr, result);
      }
    }
    solver.add(z3::implies(reach, dest == result));
  }

  void addInstructionConstraints(const IRFunction &function) {
    for (const auto &block : function.blocks) {
      z3::expr reach = blockReach.at(block.id.id);

      for (const auto &instr : block.instructions) {
        if (std::holds_alternative<BinaryInstruction>(instr)) {
          addBinaryConstraint(reach, std::get<BinaryInstruction>(instr));

        } else if (std::holds_alternative<UnaryInstruction>(instr)) {
          const auto &ui = std::get<UnaryInstruction>(instr);
          if (ui.dest.type != IRType::Void)
            solver.add(z3::implies(reach, regs.at(ui.dest.id) ==
                                              !operandToExpr(ui.src)));

        } else if (std::holds_alternative<AssignmentInstruction>(instr)) {
          const auto &ai = std::get<AssignmentInstruction>(instr);
          if (ai.dest.type != IRType::Void)
            solver.add(z3::implies(reach, regs.at(ai.dest.id) ==
                                              operandToExpr(ai.src)));

        } else if (std::holds_alternative<HavocInstruction>(instr)) {
          // Havoc leaves the destination unconstrained.

        } else if (std::holds_alternative<PhiInstruction>(instr)) {
          addPhiConstraint(reach, std::get<PhiInstruction>(instr), block.id.id);

        } else if (std::holds_alternative<ContractAssumeInstruction>(instr)) {
          const auto &ca = std::get<ContractAssumeInstruction>(instr);
          solver.add(z3::implies(reach, operandToExpr(ca.src)));

        } else if (std::holds_alternative<ContractCheckInstruction>(instr)) {
          const auto &cc = std::get<ContractCheckInstruction>(instr);
          checks.push_back({reach, operandToExpr(cc.src), instr.location});
        }
      }
    }
  }

  CounterExample extractCounterExample(const z3::model &model,
                                       const IRFunction &function,
                                       const CheckInfo &check) {
    CounterExample ce{check.location, {}, {}, std::nullopt};

    for (const auto &param : function.parameters) {
      if (param.reg.type == IRType::Void)
        continue;
      auto regIt = regs.find(param.reg.id);
      if (regIt == regs.end())
        continue;
      z3::expr val = model.eval(regIt->second, true);
      auto loc = function.registerLocation(param.reg);
      ce.parameters.push_back(
          {loc.value_or(SourceLocation::empty()), param.name, val.to_string()});
    }

    for (const auto &[regId, expr] : regs) {
      if (paramRegIds.count(regId))
        continue;
      auto loc = function.registerLocation(Register{regId, IRType::Void});
      if (!loc.has_value())
        continue;
      z3::expr val = model.eval(expr, true);
      ce.intermediates.push_back({*loc, val.to_string()});
    }

    return ce;
  }

  struct VerifyResult {
    bool anyFailed = false;
    bool anyUnknown = false;
    std::vector<CounterExample> counterExamples;
  };

  VerifyResult verifyChecks(const IRFunction &function) {
    VerifyResult vr;

    for (const auto &check : checks) {
      solver.push();
      solver.add(check.reachable && !check.condition);

      z3::check_result res = solver.check();
      if (res == z3::sat) {
        vr.anyFailed = true;
        z3::model model = solver.get_model();
        vr.counterExamples.push_back(
            extractCounterExample(model, function, check));
      } else if (res == z3::unknown) {
        vr.anyUnknown = true;
      }

      solver.pop();
    }

    return vr;
  }
};

} // anonymous namespace

blaze::backend::VcBackend::VerificationResult blaze::backend::Z3Backend::verify(
    const std::vector<frontend::IRFunction> &functions) {

  m_currentStatus = Status::Unknown;
  m_counterExamples.clear();

  for (const auto &function : functions) {
    verifyFunction(function);
  }

  VerificationResult result;
  result.status = m_currentStatus;
  result.counterExamples = std::move(m_counterExamples);
  return result;
}

void blaze::backend::Z3Backend::verifyFunction(
    const frontend::IRFunction &function) {
  FunctionVerifier fv;

  fv.createRegisterVariables(function);
  fv.buildReachability(function);
  fv.addEdgeConstraints(function);
  fv.addInstructionConstraints(function);

  auto result = fv.verifyChecks(function);

  for (auto &ce : result.counterExamples)
    m_counterExamples.push_back(std::move(ce));

  if (result.anyFailed) {
    m_currentStatus = Status::Disproven;
  } else if (result.anyUnknown) {
    if (m_currentStatus != Status::Disproven)
      m_currentStatus = Status::VerificationFailed;
  } else {
    if (m_currentStatus == Status::Unknown)
      m_currentStatus = Status::Verified;
  }
}
