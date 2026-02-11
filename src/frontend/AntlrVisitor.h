#pragma once

#include "BlazeBaseVisitor.h"

#include "core/Source.h"
#include "frontend/Ast.h"

#include <memory>

namespace blaze::frontend {

class BlazeVisitorImpl : public BlazeBaseVisitor {
public:
  explicit BlazeVisitorImpl(std::shared_ptr<core::Source> source)
      : m_source(std::move(source)) {}

  virtual std::any visitProgram(BlazeParser::ProgramContext *ctx) override;
  virtual std::any visitTypeName(BlazeParser::TypeNameContext *ctx) override;
  virtual std::any visitBlock(BlazeParser::BlockContext *ctx) override;
  virtual std::any visitStmt(BlazeParser::StmtContext *ctx) override;
  virtual std::any visitDeclStmt(BlazeParser::DeclStmtContext *ctx) override;
  virtual std::any
  visitReturnStmt(BlazeParser::ReturnStmtContext *ctx) override;
  virtual std::any visitIfStmt(BlazeParser::IfStmtContext *ctx) override;
  virtual std::any visitWhileStmt(BlazeParser::WhileStmtContext *ctx) override;
  virtual std::any
  visitAssignmentStmt(BlazeParser::AssignmentStmtContext *ctx) override;
  virtual std::any visitExprStmt(BlazeParser::ExprStmtContext *ctx) override;
  virtual std::any visitExpr(BlazeParser::ExprContext *ctx) override;
  virtual std::any visitCompExpr(BlazeParser::CompExprContext *ctx) override;
  virtual std::any visitAddExpr(BlazeParser::AddExprContext *ctx) override;
  virtual std::any visitMulExpr(BlazeParser::MulExprContext *ctx) override;
  virtual std::any visitUnaryExpr(BlazeParser::UnaryExprContext *ctx) override;
  virtual std::any visitCallExpr(BlazeParser::CallExprContext *ctx) override;
  virtual std::any visitVarExpr(BlazeParser::VarExprContext *ctx) override;
  virtual std::any visitIntExpr(BlazeParser::IntExprContext *ctx) override;
  virtual std::any visitBoolExpr(BlazeParser::BoolExprContext *ctx) override;
  virtual std::any visitParenExpr(BlazeParser::ParenExprContext *ctx) override;
  virtual std::any visitArgList(BlazeParser::ArgListContext *ctx) override;
  virtual std::any
  visitFunctionDecl(BlazeParser::FunctionDeclContext *ctx) override;
  virtual std::any visitParamList(BlazeParser::ParamListContext *ctx) override;
  virtual std::any visitParam(BlazeParser::ParamContext *ctx) override;

private:
  std::shared_ptr<Root> m_currentRoot;
  std::shared_ptr<core::Source> m_source;

protected:
  static core::SourceLocation
  sourceLocation(const antlr4::ParserRuleContext *ctx,
                 const core::Source &source);
};

} // namespace blaze::frontend
