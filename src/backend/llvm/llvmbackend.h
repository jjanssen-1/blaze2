#pragma once

#include "backend/CodeBackend.h"
#include "frontend/Ir.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "core/Errors.h"

#include <memory>
#include <string>
#include <vector>

namespace blaze::backend {

class LLVMBackend : public CodeBackend {
public:
  explicit LLVMBackend(core::DiagnosticList &diagnostics);
  ~LLVMBackend() override = default;

  Status generate(const std::vector<frontend::IRFunction> &functions) override;

  llvm::Module *module() { return m_module.get(); }
  const llvm::Module *module() const { return m_module.get(); }

  bool emitIRToFile(const std::string &path,
                    std::string *error = nullptr) const;
  bool emitObjectFile(const std::string &path, std::string *error = nullptr);
  bool emitExecutable(const std::string &outputPath,
                      const std::vector<std::string> &objectFiles,
                      const std::string &linkerPath,
                      const std::vector<std::string> &linkerArgs,
                      std::string *error = nullptr) const;

private:
  std::unique_ptr<llvm::LLVMContext> m_context;
  std::unique_ptr<llvm::Module> m_module;
  std::unique_ptr<llvm::IRBuilder<>> m_builder;
  std::unordered_map<frontend::IRType, llvm::Type *> m_typeMap;
  core::DiagnosticList &m_diagnostics;
  std::unordered_map<core::u64, llvm::BasicBlock *> m_blockMap;
  std::unordered_map<core::u64, llvm::Value *> m_regValues;
  llvm::Value *valueOf(const frontend::Operand &op,
                       const core::SourceLocation &loc);

  // Utility
  llvm::Type *llvmType(frontend::IRType type);
  std::vector<llvm::Type *>
  llvmTypes(const std::vector<frontend::IRParam> &types);

  // IR traversal
  llvm::Function *declareFunction(const frontend::IRFunction &func);
  void emitBlocks(const frontend::IRFunction &func, llvm::Function *llvmFunc);
  void emitInstruction(const frontend::Instruction &instr);
  void emitTerminator(const frontend::Terminator &term,
                      llvm::Function *llvmFunc);
};

} // namespace blaze::backend
