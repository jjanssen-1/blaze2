#pragma once

#include "backend/CodeBackend.h"

#include <memory>
#include <string>
#include <vector>

namespace llvm {
class LLVMContext;
class Module;
} // namespace llvm

namespace blaze::backend {

class LLVMBackend : public CodeBackend {
public:
  LLVMBackend();
  ~LLVMBackend() override;

  Status generate(const std::vector<frontend::IRFunction> &functions) override;

  llvm::Module *module();
  const llvm::Module *module() const;

  std::string irString() const;
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
};

} // namespace blaze::backend
