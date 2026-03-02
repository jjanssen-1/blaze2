#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <vector>

#include "driver/CompileOptions.h"
#include "driver/Driver.h"

using namespace blaze::driver;

namespace {

std::string findBlazeExecutable() {
  std::vector<std::filesystem::path> candidates = {
      std::filesystem::current_path() / "bin" / "blaze",
      std::filesystem::current_path() / ".." / "bin" / "blaze",
  };
  for (const auto &candidate : candidates) {
    if (std::filesystem::exists(candidate)) {
      return candidate.string();
    }
  }
  return {};
}

} // namespace

class DriverTest : public ::testing::Test {
protected:
  void SetUp() override {
    createFile("test_ok.blz",
               "fn id(x: i32) -> i32 post { r: r == x; } { return x; }");
    createFile("test_bad.blz",
               "fn bad(x: i32) -> i32 post { r: r == x + 1; } { return x; }");
    createFile("cli_ok.blz", "fn main() -> i32 { return 0; }");
  }

  void TearDown() override {
    std::remove("test_ok.blz");
    std::remove("test_bad.blz");
    std::remove("test_ok.ll");
    std::remove("test_ok.o");
    std::remove("cli_ok.blz");
    std::remove("cli_ok.ll");
    std::remove("cli_ok.o");
    std::remove("cli_ok");
  }

  void createFile(const std::string &path, const std::string &content) {
    std::ofstream out(path);
    out << content;
  }

  std::string readFile(const std::string &path) {
    std::ifstream in(path);
    std::string content((std::istreambuf_iterator<char>(in)),
                        std::istreambuf_iterator<char>());
    return content;
  }
};

TEST_F(DriverTest, VerifyOnlySuccess) {
  CompileOptions opts;
  opts.inputFile = "test_ok.blz";
  opts.verify = true;
  opts.verifyOnly = true;

  DriverResult result = run(opts);
  EXPECT_EQ(result.status, DriverResult::Ok);
  EXPECT_FALSE(result.diagnostics.hasErrors());
}

TEST_F(DriverTest, VerifyOnlyFailure) {
  CompileOptions opts;
  opts.inputFile = "test_bad.blz";
  opts.verify = true;
  opts.verifyOnly = true;

  DriverResult result = run(opts);
  EXPECT_EQ(result.status, DriverResult::VerificationFailed);
}

TEST_F(DriverTest, EmitLLVM) {
  CompileOptions opts;
  opts.inputFile = "test_ok.blz";
  opts.outputKind = OutputKind::EmitLLVM;
  opts.outputFile = "test_ok.ll";
  opts.verify = false;

  DriverResult result = run(opts);
  EXPECT_EQ(result.status, DriverResult::Ok);
  EXPECT_FALSE(result.diagnostics.hasErrors());

  std::string llContent = readFile("test_ok.ll");
  EXPECT_NE(llContent.find("define"), std::string::npos);
}

TEST_F(DriverTest, EmitObject) {
  CompileOptions opts;
  opts.inputFile = "test_ok.blz";
  opts.outputKind = OutputKind::EmitObject;
  opts.outputFile = "test_ok.o";
  opts.verify = false;

  DriverResult result = run(opts);
  EXPECT_EQ(result.status, DriverResult::Ok);
  EXPECT_FALSE(result.diagnostics.hasErrors());
  EXPECT_TRUE(std::filesystem::exists("test_ok.o"));
}

TEST_F(DriverTest, CliDefaultBuildAndVerify) {
  std::string blazePath = findBlazeExecutable();
  if (blazePath.empty()) {
    GTEST_SKIP() << "blaze executable not found";
  }

  std::string cmd = "\"" + blazePath + "\" \"cli_ok.blz\"";
  int code = std::system(cmd.c_str());
  EXPECT_EQ(code, 0);
  EXPECT_TRUE(std::filesystem::exists("cli_ok"));
}

TEST_F(DriverTest, CliEmitLLVMFlag) {
  std::string blazePath = findBlazeExecutable();
  if (blazePath.empty()) {
    GTEST_SKIP() << "blaze executable not found";
  }

  std::string cmd =
      "\"" + blazePath + "\" --emit-llvm \"cli_ok.blz\" -o \"cli_ok.ll\"";
  int code = std::system(cmd.c_str());
  EXPECT_EQ(code, 0);
  EXPECT_TRUE(std::filesystem::exists("cli_ok.ll"));
}

TEST_F(DriverTest, CliCompileOnlyFlag) {
  std::string blazePath = findBlazeExecutable();
  if (blazePath.empty()) {
    GTEST_SKIP() << "blaze executable not found";
  }

  std::string cmd =
      "\"" + blazePath + "\" --compile-only \"cli_ok.blz\" -o \"cli_ok.o\"";
  int code = std::system(cmd.c_str());
  EXPECT_EQ(code, 0);
  EXPECT_TRUE(std::filesystem::exists("cli_ok.o"));
}
