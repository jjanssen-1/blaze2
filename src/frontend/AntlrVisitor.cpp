#include "AntlrVisitor.h"

#include "core/Errors.h"
#include "core/Source.h"

#include "frontend/Ast.h"

#include <charconv>
#include <fmt/format.h>
#include <memory>
#include <string>
#include <vector>

namespace blaze::frontend {

core::SourceLocation
BlazeVisitorImpl::sourceLocation(const antlr4::ParserRuleContext *ctx,
                                 const core::Source &source) {
  if (!ctx || !ctx->getStart()) {
    return core::SourceLocation::empty();
  }

  const auto *start = ctx->getStart();
  const auto *stop = ctx->getStop();

  const core::size startIndex = static_cast<core::size>(start->getStartIndex());
  core::size length = 0;
  if (stop) {
    const auto stopIndex = static_cast<core::size>(stop->getStopIndex());
    if (stopIndex >= startIndex) {
      length = (stopIndex - startIndex) + 1;
    }
  }

  const auto view = source.view(startIndex, length);
  const core::size line = static_cast<core::size>(start->getLine());
  const core::size column =
      static_cast<core::size>(start->getCharPositionInLine());
  return core::SourceLocation(view, line, column);
}

std::any BlazeVisitorImpl::visitProgram(BlazeParser::ProgramContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  m_currentRoot = std::make_shared<Root>(loc);

  for (auto &fn : ctx->functionDecl()) {
    m_currentRoot->functions.push_back(
        std::any_cast<Function>(fn->accept(this)));
  }

  return m_currentRoot;
}

std::any BlazeVisitorImpl::visitTypeName(BlazeParser::TypeNameContext *ctx) {
  auto *identNode = ctx->Identifier();
  if (!identNode) {
    // TODO: report Internal error.
  }
  Identifier identifier{identNode ? identNode->getText() : std::string{}};
  return TypeName{identifier};
}
std::any BlazeVisitorImpl::visitBlock(BlazeParser::BlockContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  Block output(loc);

  for (const auto &stmt : ctx->stmt()) {
    output.statements.push_back(std::any_cast<Statement>(stmt->accept(this)));
  }

  return output;
}
std::any BlazeVisitorImpl::visitStmt(BlazeParser::StmtContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  if (ctx->declStmt()) {
    return Statement(loc,
                     std::any_cast<DeclStmt>(ctx->declStmt()->accept(this)));
  } else if (ctx->returnStmt()) {
    return Statement(
        loc, std::any_cast<ReturnStmt>(ctx->returnStmt()->accept(this)));
  } else if (ctx->ifStmt()) {
    return Statement(loc, std::any_cast<IfStmt>(ctx->ifStmt()->accept(this)));
  } else if (ctx->whileStmt()) {
    return Statement(loc,
                     std::any_cast<WhileStmt>(ctx->whileStmt()->accept(this)));
  } else if (ctx->exprStmt()) {
    return std::any_cast<Statement>(ctx->exprStmt()->accept(this));
  } else if (ctx->block()) {
    return Statement(loc, std::make_shared<Block>(std::any_cast<Block>(
                              ctx->block()->accept(this))));
  } else if (ctx->assignmentStmt()) {
    return Statement(loc, std::any_cast<AssignmentStmt>(
                              ctx->assignmentStmt()->accept(this)));
  } else {
    // Internal error
    return std::nullopt;
  }
}
std::any BlazeVisitorImpl::visitDeclStmt(BlazeParser::DeclStmtContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);

  auto *identNode = ctx->Identifier();
  if (!identNode) {
    // TODO: report Internal error.
  }
  Identifier identifier{identNode ? identNode->getText() : std::string{}};
  bool isConstant = ctx->CONST() != nullptr;
  const TypeName type = std::any_cast<TypeName>(ctx->typeName()->accept(this));
  const ExprPtr expression = std::any_cast<ExprPtr>(ctx->expr()->accept(this));

  DeclStmt output(loc, identifier, type, isConstant, expression);

  return output;
}

std::any
BlazeVisitorImpl::visitReturnStmt(BlazeParser::ReturnStmtContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  std::optional<ExprPtr> expression;
  if (ctx->expr()) {
    expression = std::any_cast<ExprPtr>(ctx->expr()->accept(this));
  }
  ReturnStmt output(loc, expression);

  return output;
}
std::any BlazeVisitorImpl::visitIfStmt(BlazeParser::IfStmtContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  const ExprPtr condition = std::any_cast<ExprPtr>(ctx->expr()->accept(this));
  auto consequentBlock = std::make_shared<Block>(
      std::any_cast<Block>(ctx->block(0)->accept(this)));
  const StmtPtr consequent = std::make_shared<Statement>(
      consequentBlock->location, BlockPtr(consequentBlock));
  std::optional<StmtPtr> alternative;
  if (ctx->ELSE()) {
    if (ctx->ifStmt()) {
      // else if
      auto elseIf = std::any_cast<IfStmt>(ctx->ifStmt()->accept(this));
      alternative =
          std::make_shared<Statement>(elseIf.location, std::move(elseIf));
    } else {
      // simple else
      auto elseBlock = std::make_shared<Block>(
          std::any_cast<Block>(ctx->block(1)->accept(this)));
      alternative =
          std::make_shared<Statement>(elseBlock->location, BlockPtr(elseBlock));
    }
  }

  IfStmt output(loc, condition, consequent, alternative);

  return output;
}
std::any BlazeVisitorImpl::visitWhileStmt(BlazeParser::WhileStmtContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  const ExprPtr condition = std::any_cast<ExprPtr>(ctx->expr()->accept(this));

  auto bodyBlock =
      std::make_shared<Block>(std::any_cast<Block>(ctx->block()->accept(this)));
  const StmtPtr body =
      std::make_shared<Statement>(bodyBlock->location, BlockPtr(bodyBlock));

  WhileStmt output(loc, condition, body);
  return output;
}
std::any
BlazeVisitorImpl::visitAssignmentStmt(BlazeParser::AssignmentStmtContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  const ExprPtr rhs = std::any_cast<ExprPtr>(ctx->expr()->accept(this));

  auto identNode = ctx->Identifier();
  Identifier identifier{identNode ? identNode->getText() : std::string{}};
  AssignmentStmt output(loc, identifier, rhs);
  return output;
}
std::any BlazeVisitorImpl::visitExprStmt(BlazeParser::ExprStmtContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  return Statement(loc, std::any_cast<ExprPtr>(ctx->expr()->accept(this)));
}
std::any BlazeVisitorImpl::visitExpr(BlazeParser::ExprContext *ctx) {
  return ctx->compExpr()->accept(this);
}

std::any BlazeVisitorImpl::visitCompExpr(BlazeParser::CompExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);

  auto current = std::any_cast<ExprPtr>(ctx->addExpr(0)->accept(this));
  if (ctx->addExpr().size() > 1) {
    auto rhs = std::any_cast<ExprPtr>(ctx->addExpr(1)->accept(this));
    BinaryOperation op;
    if (ctx->LT()) {
      op = BinaryOperation::LessThan;
    } else if (ctx->LE()) {
      op = BinaryOperation::LessEqual;
    } else if (ctx->GT()) {
      op = BinaryOperation::GreaterThan;
    } else if (ctx->GE()) {
      op = BinaryOperation::GreaterEqual;
    } else if (ctx->EQ_EQ()) {
      op = BinaryOperation::Equal;
    } else {
      op = BinaryOperation::NotEqual;
    }
    BinaryExpr expr(op, current, rhs);
    current = std::make_shared<Expression>(loc, expr);
  }

  return current;
}
std::any BlazeVisitorImpl::visitAddExpr(BlazeParser::AddExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);

  auto current = std::any_cast<ExprPtr>(ctx->mulExpr(0)->accept(this));
  for (size_t i = 1; i < ctx->mulExpr().size(); ++i) {
    auto rhs = std::any_cast<ExprPtr>(ctx->mulExpr(i)->accept(this));
    const BinaryOperation op = ctx->getToken(BlazeParser::PLUS, i - 1)
                                   ? BinaryOperation::Addition
                                   : BinaryOperation::Subtraction;
    BinaryExpr expr(op, current, rhs);
    current = std::make_shared<Expression>(loc, expr);
  }

  return current;
}

std::any BlazeVisitorImpl::visitMulExpr(BlazeParser::MulExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);

  auto current = std::any_cast<ExprPtr>(ctx->unaryExpr(0)->accept(this));
  for (size_t i = 1; i < ctx->unaryExpr().size(); ++i) {
    auto rhs = std::any_cast<ExprPtr>(ctx->unaryExpr(i)->accept(this));
    const BinaryOperation op = ctx->getToken(BlazeParser::STAR, i - 1)
                                   ? BinaryOperation::Multiplication
                                   : BinaryOperation::Division;
    BinaryExpr expr(op, current, rhs);
    current = std::make_shared<Expression>(loc, expr);
  }

  return current;
}

std::any BlazeVisitorImpl::visitUnaryExpr(BlazeParser::UnaryExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  if (ctx->primaryExpr()) {
    return ctx->primaryExpr()->accept(this);
  }

  auto operand = std::any_cast<ExprPtr>(ctx->unaryExpr()->accept(this));
  if (!ctx->getToken(BlazeParser::NOT, 0)) {
    return operand;
  }
  UnaryExpr expr(UnaryOperation::Negation, operand);
  return std::make_shared<Expression>(loc, expr);
}

std::any BlazeVisitorImpl::visitCallExpr(BlazeParser::CallExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  auto *identNode = ctx->Identifier();
  if (!identNode) {
    // TODO: report Internal error.
  }

  Identifier identifier{identNode ? identNode->getText() : std::string{}};
  CallExpr output(identifier);

  if (ctx->argList()) {
    auto arguments =
        std::any_cast<std::vector<ExprPtr>>(ctx->argList()->accept(this));
    output.arguments = std::move(arguments);
  }

  return std::make_shared<Expression>(loc, output);
}

std::any BlazeVisitorImpl::visitVarExpr(BlazeParser::VarExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  auto *identNode = ctx->Identifier();
  if (!identNode) {
    // TODO: report Internal error.
  }

  Identifier identifier{identNode ? identNode->getText() : std::string{}};
  VarExpr output(identifier);
  return std::make_shared<Expression>(loc, output);
}

core::u64 BlazeVisitorImpl::parseNumber(const std::string &numStr,
                                        const core::SourceLocation &loc) {
  int base = 10, offset = 0;
  if (numStr.size() < 2) {
    // One digit decimal
  } else if (numStr[0] == '0' && numStr[1] == 'x') {
    // 0x -> Hex
    base = 16;
    offset = 2;
  } else if (numStr[0] == '0' && numStr[1] == 'b') {
    // 0b -> Binary
    base = 2;
    offset = 2;
  } else if (numStr[0] == '0' && numStr[1] == 'b') {
    // 0o -> Oct
    base = 8;
    offset = 2;
  }
  std::string cleaned = numStr;
  std::erase(cleaned, '\'');
  core::u64 output;
  auto result = std::from_chars(cleaned.data() + offset,
                                cleaned.data() + cleaned.size(), output, base);

  if (result.ec == std::errc::invalid_argument) {
    m_diagnostics.reportError(
        core::ERROR_WRONG_NUMBER_FORMAT,
        fmt::format("Number contains invalid character: {0}", numStr), loc);
    return 0;
  }

  if (result.ec == std::errc::result_out_of_range) {
    m_diagnostics.reportError(
        core::ERROR_WRONG_NUMBER_FORMAT,
        fmt::format("Number string is too large for a string literal: {0}",
                    numStr),
        loc);
    return 0;
  }

  return output;
}

std::any BlazeVisitorImpl::visitIntExpr(BlazeParser::IntExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  auto *token = ctx->Integer();
  if (!token) {
    // TODO: report Internal error.
    return std::make_shared<Expression>(loc, IntExpr(0));
  }

  core::u64 value = parseNumber(token->getText(), loc);
  IntExpr output(value);
  return std::make_shared<Expression>(loc, output);
}

std::any BlazeVisitorImpl::visitBoolExpr(BlazeParser::BoolExprContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  const bool value = ctx->TRUE() != nullptr;
  BoolExpr output(value);
  return std::make_shared<Expression>(loc, output);
}

std::any BlazeVisitorImpl::visitParenExpr(BlazeParser::ParenExprContext *ctx) {
  return ctx->expr()->accept(this);
}

std::any BlazeVisitorImpl::visitArgList(BlazeParser::ArgListContext *ctx) {
  std::vector<ExprPtr> arguments;
  for (const auto &expr : ctx->expr()) {
    arguments.push_back(std::any_cast<ExprPtr>(expr->accept(this)));
  }
  return arguments;
}
std::any
BlazeVisitorImpl::visitFunctionDecl(BlazeParser::FunctionDeclContext *ctx) {
  auto loc = sourceLocation(ctx, *m_source);
  Block body = std::any_cast<Block>(ctx->block()->accept(this));
  std::vector<ExprPtr> preConditions, postConditions;
  std::optional<Identifier> postResultBinding;

  for (const auto &specification : ctx->fnSpec()) {
    std::vector<ExprPtr> *specBody = nullptr;
    if (specification->PRE()) {
      specBody = &preConditions;
    } else if (specification->POST()) {
      specBody = &postConditions;
      // Capture optional result binding: post(r) { ... }
      if (specification->Identifier()) {
        postResultBinding.emplace(
            Identifier{specification->Identifier()->getText()});
      }
    } else {
      // TODO: report Internal error.
    }

    for (const auto &expression : specification->exprStmt()) {
      auto exprPtr = std::any_cast<ExprPtr>(expression->expr()->accept(this));
      if (exprPtr) {
        specBody->push_back(exprPtr);
      } else {
        // TODO: report Internal error.
      }
    }
  }

  auto *identNode = ctx->Identifier();
  if (!identNode) {
    // TODO: report Internal error.
  }
  Identifier identifier{identNode ? identNode->getText() : std::string{}};

  std::vector<Param> parameters;
  if (ctx->paramList()) {
    auto parsedParams =
        std::any_cast<std::vector<Param>>(ctx->paramList()->accept(this));
    parameters = std::move(parsedParams);
  }

  std::optional<TypeName> returnType;
  if (ctx->typeName()) {
    returnType.emplace(std::any_cast<TypeName>(ctx->typeName()->accept(this)));
  }

  FunctionSpecifications specs;
  specs.pre = std::move(preConditions);
  specs.post = std::move(postConditions);
  specs.postResultBinding = std::move(postResultBinding);

  Function output(loc, identifier, parameters, returnType, body, specs);
  return output;
}
std::any BlazeVisitorImpl::visitParamList(BlazeParser::ParamListContext *ctx) {
  std::vector<Param> params;
  for (const auto &paramCtx : ctx->param()) {
    params.push_back(std::any_cast<Param>(paramCtx->accept(this)));
  }
  return params;
}

std::any BlazeVisitorImpl::visitParam(BlazeParser::ParamContext *ctx) {
  auto *identNode = ctx->Identifier();
  if (!identNode) {
    // TODO: report Internal error.
  }
  Identifier identifier{identNode ? identNode->getText() : std::string{}};
  const TypeName type = std::any_cast<TypeName>(ctx->typeName()->accept(this));
  return Param{identifier, type};
}

} // namespace blaze::frontend
