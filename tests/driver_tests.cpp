#include <cstdio>
#include <fstream>
#include <gtest/gtest.h>
#include <string>

#include "driver/CompileOptions.h"
#include "driver/Driver.h"

using namespace blaze::driver;

class DriverTest : public ::testing::Test {
protected:
  void SetUp() override {
    createFile("test_ok.blz",
               "fn id(x: i32) -> i32 post { r: r == x; } { return x; }");
    createFile("test_bad.blz",
               "fn bad(x: i32) -> i32 post { r: r == x + 1; } { return x; }");
  }

  void TearDown() override {
    std::remove("test_ok.blz");
    std::remove("test_bad.blz");
    std::remove("test_ok.ll");
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
