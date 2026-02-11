#include <gtest/gtest.h>

#include "core/Errors.h"
#include "core/Source.h"
#include "frontend/FrontendDriver.h"

namespace blaze::frontend::tests {

namespace {

// Helper: run the parse pipeline and return the AST root and source for
// inspection.
struct ParseOutcome {
  std::shared_ptr<Root> root;
  std::shared_ptr<core::Source> source;
};

ParseOutcome parse(const std::string &code,
                   const std::string &filename = "ast_parse_test.blz") {
  auto source = std::make_shared<core::Source>(code, filename);
  core::DiagnosticList diagnostics;
  auto result = parseSource(source, diagnostics);
  if (!result || diagnostics.hasErrors()) {
    return {nullptr, std::move(source)};
  }
  return {result.value().root, std::move(source)};
}

const Expression *getExpr(const ExprPtr &exprPtr) {
  return exprPtr ? exprPtr.get() : nullptr;
}

} // namespace

// ---------------------------------------------------------------------------
// Comprehensive node coverage
// ---------------------------------------------------------------------------

TEST(AstParsing, CoversAllNodes) {
  auto [root, source] = parse("fn foo(a: int, b: int) -> int "
                              "pre { a; } post { b; } "
                              "{"
                              "  var x: int = 1 + 2 * 3;"
                              "  (1 + 2);"
                              "  foo(a, b);"
                              "  if (x) { return x; } else { return 0; }"
                              "  while (x) { x; }"
                              "  return !x;"
                              "}");
  ASSERT_TRUE(root);
  ASSERT_EQ(root->functions.size(), 1u);

  const auto &fn = root->functions.front();
  EXPECT_EQ(fn.identifier.name, "foo");
  ASSERT_EQ(fn.parameters.size(), 2u);
  EXPECT_EQ(fn.parameters[0].identifier.name, "a");
  EXPECT_EQ(fn.parameters[0].type.identifier.name, "int");
  EXPECT_EQ(fn.parameters[1].identifier.name, "b");
  EXPECT_EQ(fn.parameters[1].type.identifier.name, "int");
  ASSERT_TRUE(fn.returnType.has_value());
  EXPECT_EQ(fn.returnType->identifier.name, "int");

  ASSERT_EQ(fn.specifications.pre.size(), 1u);
  ASSERT_EQ(fn.specifications.post.size(), 1u);
  // No result binding in `post { b; }`
  EXPECT_FALSE(fn.specifications.postResultBinding.has_value());

  const auto &body = fn.body;
  ASSERT_GE(body.statements.size(), 6u);
}

// ---------------------------------------------------------------------------
// Declaration statements
// ---------------------------------------------------------------------------

TEST(AstParsing, DeclStmtNode) {
  auto [root, source] =
      parse("fn foo(a: int) -> int { var x: int = 1 + 2; return x; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_FALSE(body.statements.empty());
  ASSERT_TRUE(std::holds_alternative<DeclStmt>(body.statements[0]));

  const auto &decl = std::get<DeclStmt>(body.statements[0]);
  EXPECT_EQ(decl.identifier.name, "x");
  EXPECT_EQ(decl.type.identifier.name, "int");
  const auto *expr = getExpr(decl.assignedExpression);
  ASSERT_TRUE(expr);
  EXPECT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
}

// ---------------------------------------------------------------------------
// Expression statements
// ---------------------------------------------------------------------------

TEST(AstParsing, ExprStmtBinaryExpr) {
  auto [root, source] = parse("fn foo(a: int) -> int { (1 + 2); return a; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_GE(body.statements.size(), 2u);
  ASSERT_TRUE(std::holds_alternative<ExprPtr>(body.statements[0]));

  const auto exprStmt = std::get<ExprPtr>(body.statements[0]);
  const auto *expr = getExpr(exprStmt);
  ASSERT_TRUE(expr);
  EXPECT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
}

TEST(AstParsing, VarExprNode) {
  auto [root, source] = parse("fn foo(a: int) -> int { a; return a; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ExprPtr>(body.statements[0]));

  const auto exprStmt = std::get<ExprPtr>(body.statements[0]);
  const auto *expr = getExpr(exprStmt);
  ASSERT_TRUE(expr);
  EXPECT_TRUE(std::holds_alternative<VarExpr>(*expr));
}

TEST(AstParsing, IntExprNode) {
  auto [root, source] = parse("fn foo(a: int) -> int { 1; return 0; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ExprPtr>(body.statements[0]));

  const auto exprStmt = std::get<ExprPtr>(body.statements[0]);
  const auto *expr = getExpr(exprStmt);
  ASSERT_TRUE(expr);
  EXPECT_TRUE(std::holds_alternative<IntExpr>(*expr));
}

TEST(AstParsing, BoolExprNode) {
  auto [root, source] = parse("fn foo(a: int) -> int { true; return 0; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ExprPtr>(body.statements[0]));

  const auto exprStmt = std::get<ExprPtr>(body.statements[0]);
  const auto *expr = getExpr(exprStmt);
  ASSERT_TRUE(expr);
  EXPECT_TRUE(std::holds_alternative<BoolExpr>(*expr));

  const auto &boolExpr = std::get<BoolExpr>(*expr);
  EXPECT_TRUE(boolExpr.value);
}

// ---------------------------------------------------------------------------
// Call expressions
// ---------------------------------------------------------------------------

TEST(AstParsing, CallExprNode) {
  auto [root, source] = parse("fn foo(a: int) -> int { foo(a); return a; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ExprPtr>(body.statements[0]));

  const auto exprStmt = std::get<ExprPtr>(body.statements[0]);
  const auto *expr = getExpr(exprStmt);
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<CallExpr>(*expr));

  const auto &call = std::get<CallExpr>(*expr);
  EXPECT_EQ(call.identifier.name, "foo");
}

// ---------------------------------------------------------------------------
// Control-flow statements
// ---------------------------------------------------------------------------

TEST(AstParsing, IfStmtNode) {
  auto [root, source] = parse(
      "fn foo(a: int) -> int { if (a) { return a; } else { return 0; } }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<IfStmt>(body.statements[0]));
}

TEST(AstParsing, WhileStmtNode) {
  auto [root, source] =
      parse("fn foo(a: int) -> int { while (a) { a; } return a; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<WhileStmt>(body.statements[0]));
}

// ---------------------------------------------------------------------------
// Return statements
// ---------------------------------------------------------------------------

TEST(AstParsing, ReturnStmtUnaryExpr) {
  auto [root, source] = parse("fn foo(a: int) -> int { return !a; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));

  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  ASSERT_TRUE(retStmt.expression.has_value());

  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  EXPECT_TRUE(std::holds_alternative<UnaryExpr>(*expr));
}

// ---------------------------------------------------------------------------
// Post result binding: post(r) { ... }
// ---------------------------------------------------------------------------

TEST(AstParsing, PostResultBindingParsed) {
  auto [root, source] = parse("fn foo(a: int) -> int "
                              "post { r : r; } "
                              "{ return a; }");
  ASSERT_TRUE(root);
  ASSERT_EQ(root->functions.size(), 1u);

  const auto &fn = root->functions.front();
  ASSERT_EQ(fn.specifications.post.size(), 1u);
  ASSERT_TRUE(fn.specifications.postResultBinding.has_value());
  EXPECT_EQ(fn.specifications.postResultBinding->name, "r");
}

TEST(AstParsing, PostWithoutResultBinding) {
  auto [root, source] = parse("fn foo(a: int) -> int "
                              "post { a; } "
                              "{ return a; }");
  ASSERT_TRUE(root);
  ASSERT_EQ(root->functions.size(), 1u);

  const auto &fn = root->functions.front();
  ASSERT_EQ(fn.specifications.post.size(), 1u);
  EXPECT_FALSE(fn.specifications.postResultBinding.has_value());
}

TEST(AstParsing, PostResultBindingCustomName) {
  auto [root, source] = parse("fn foo(x: int) -> int "
                              "post { result : result; } "
                              "{ return x; }");
  ASSERT_TRUE(root);
  ASSERT_EQ(root->functions.size(), 1u);

  const auto &fn = root->functions.front();
  ASSERT_TRUE(fn.specifications.postResultBinding.has_value());
  EXPECT_EQ(fn.specifications.postResultBinding->name, "result");
}

TEST(AstParsing, PreAndPostWithResultBinding) {
  auto [root, source] = parse("fn foo(a: int) -> int "
                              "pre { a; } "
                              "post { r : r; } "
                              "{ return a; }");
  ASSERT_TRUE(root);
  ASSERT_EQ(root->functions.size(), 1u);

  const auto &fn = root->functions.front();
  ASSERT_EQ(fn.specifications.pre.size(), 1u);
  ASSERT_EQ(fn.specifications.post.size(), 1u);
  ASSERT_TRUE(fn.specifications.postResultBinding.has_value());
  EXPECT_EQ(fn.specifications.postResultBinding->name, "r");
}

TEST(AstParsing, PostResultBindingMultipleExprs) {
  auto [root, source] = parse("fn foo(a: int, b: int) -> int "
                              "post { r : r; a; } "
                              "{ return a; }");
  ASSERT_TRUE(root);
  ASSERT_EQ(root->functions.size(), 1u);

  const auto &fn = root->functions.front();
  ASSERT_EQ(fn.specifications.post.size(), 2u);
  ASSERT_TRUE(fn.specifications.postResultBinding.has_value());
  EXPECT_EQ(fn.specifications.postResultBinding->name, "r");
}

// ---------------------------------------------------------------------------
// Comparison expressions
// ---------------------------------------------------------------------------

TEST(AstParsing, CompExprLessThan) {
  auto [root, source] =
      parse("fn foo(a: i32, b: i32) -> bool { return a < b; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  ASSERT_TRUE(retStmt.expression.has_value());
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
  EXPECT_EQ(std::get<BinaryExpr>(*expr).operation, BinaryOperation::LessThan);
}

TEST(AstParsing, CompExprLessEqual) {
  auto [root, source] =
      parse("fn foo(a: i32, b: i32) -> bool { return a <= b; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
  EXPECT_EQ(std::get<BinaryExpr>(*expr).operation, BinaryOperation::LessEqual);
}

TEST(AstParsing, CompExprGreaterThan) {
  auto [root, source] =
      parse("fn foo(a: i32, b: i32) -> bool { return a > b; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
  EXPECT_EQ(std::get<BinaryExpr>(*expr).operation,
            BinaryOperation::GreaterThan);
}

TEST(AstParsing, CompExprGreaterEqual) {
  auto [root, source] =
      parse("fn foo(a: i32, b: i32) -> bool { return a >= b; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
  EXPECT_EQ(std::get<BinaryExpr>(*expr).operation,
            BinaryOperation::GreaterEqual);
}

TEST(AstParsing, CompExprEqual) {
  auto [root, source] =
      parse("fn foo(a: i32, b: i32) -> bool { return a == b; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
  EXPECT_EQ(std::get<BinaryExpr>(*expr).operation, BinaryOperation::Equal);
}

TEST(AstParsing, CompExprNotEqual) {
  auto [root, source] =
      parse("fn foo(a: i32, b: i32) -> bool { return a != b; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
  EXPECT_EQ(std::get<BinaryExpr>(*expr).operation, BinaryOperation::NotEqual);
}

TEST(AstParsing, CompExprPrecedenceBelowAddition) {
  // 1 + 2 < 3 + 4 should parse as (1 + 2) < (3 + 4)
  auto [root, source] = parse("fn foo() -> bool { return 1 + 2 < 3 + 4; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));

  const auto &cmp = std::get<BinaryExpr>(*expr);
  EXPECT_EQ(cmp.operation, BinaryOperation::LessThan);
  // Both children should be addition expressions
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*cmp.left));
  EXPECT_EQ(std::get<BinaryExpr>(*cmp.left).operation,
            BinaryOperation::Addition);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*cmp.right));
  EXPECT_EQ(std::get<BinaryExpr>(*cmp.right).operation,
            BinaryOperation::Addition);
}

TEST(AstParsing, CompExprWithoutOperatorIsPassthrough) {
  // A plain additive expression with no comparison operator should
  // pass through compExpr unchanged.
  auto [root, source] = parse("fn foo() -> i32 { return 1 + 2; }");
  ASSERT_TRUE(root);

  const auto &body = root->functions.front().body;
  ASSERT_TRUE(std::holds_alternative<ReturnStmt>(body.statements[0]));
  const auto &retStmt = std::get<ReturnStmt>(body.statements[0]);
  const auto *expr = getExpr(retStmt.expression.value());
  ASSERT_TRUE(expr);
  ASSERT_TRUE(std::holds_alternative<BinaryExpr>(*expr));
  EXPECT_EQ(std::get<BinaryExpr>(*expr).operation, BinaryOperation::Addition);
}

} // namespace blaze::frontend::tests
