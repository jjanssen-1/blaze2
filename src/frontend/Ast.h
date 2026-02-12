#pragma once

#include "core/Common.h"
#include "core/Source.h"
#include "frontend/SymbolId.h"

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace blaze::frontend {

struct AstNode {
  inline AstNode(const core::SourceLocation &loc) : location(loc) {}
  const core::SourceLocation location;
  virtual ~AstNode() = default;
};

typedef std::shared_ptr<AstNode> AstPtr;

// Forward declarations
struct Root;
struct Function;
struct Param;
struct TypeName;

// Statements
struct Block;
struct DeclStmt;
struct ReturnStmt;
struct IfStmt;
struct WhileStmt;
struct AssignmentStmt;

// Expressions
struct BinaryExpr;
struct UnaryExpr;
struct CallExpr;
struct VarExpr;
struct IntExpr;
struct BoolExpr;

// Expression is forward-declared as a struct so that ExprPtr (shared_ptr) can
// be used by expression types before Expression is fully defined.
struct Expression;
typedef std::shared_ptr<Expression> ExprPtr;

struct Statement;
typedef std::shared_ptr<Block> BlockPtr;
typedef std::shared_ptr<Statement> StmtPtr;

struct Identifier {
  std::string name;
  mutable std::optional<SymbolId> symbolId = std::nullopt;
  bool operator==(const Identifier &other) const { return name == other.name; }
};

struct TypeName {
  const Identifier identifier;
};

struct Param {
  const Identifier identifier;
  const TypeName type;
};

// ---- Expression types ----

enum BinaryOperation {
  Addition,
  Subtraction,
  Multiplication,
  Division,
  LessThan,
  LessEqual,
  GreaterThan,
  GreaterEqual,
  Equal,
  NotEqual
};

const char *binaryOperationToString(const BinaryOperation &op);

struct BinaryExpr {
  inline BinaryExpr(const BinaryOperation &op, const ExprPtr &lhs,
                    const ExprPtr &rhs)
      : operation(op), left(lhs), right(rhs) {}
  const BinaryOperation operation;
  const ExprPtr left, right;
};

enum UnaryOperation { Negation };

struct UnaryExpr {
  inline UnaryExpr(const UnaryOperation &op, const ExprPtr &operand)
      : operation(op), operand(operand) {}
  const UnaryOperation operation;
  const ExprPtr operand;
};

struct CallExpr {
  inline CallExpr(const Identifier &ident) : identifier(ident) {}
  Identifier identifier;
  std::vector<ExprPtr> arguments;
};

struct VarExpr {
  inline VarExpr(const Identifier &ident) : identifier(ident) {}
  Identifier identifier;
};

struct IntExpr {
  inline IntExpr(const core::u64 num) : value(num) {}
  const core::u64 value;
};

struct BoolExpr {
  inline BoolExpr(bool val) : value(val) {}
  const bool value;
};

// Expression inherits from the variant so that std::holds_alternative,
// std::get, and std::visit continue to work unchanged, while also carrying
// a location and resolvedType field.
struct Expression : public std::variant<BinaryExpr, UnaryExpr, CallExpr,
                                        VarExpr, IntExpr, BoolExpr> {
  using Variant =
      std::variant<BinaryExpr, UnaryExpr, CallExpr, VarExpr, IntExpr, BoolExpr>;

  template <typename T, typename = std::enable_if_t<
                            !std::is_same_v<std::decay_t<T>, Expression> &&
                            std::is_constructible_v<Variant, T &&>>>
  Expression(const core::SourceLocation &loc, T &&val)
      : Variant(std::forward<T>(val)), location(loc) {}

  const core::SourceLocation location;
  mutable std::optional<SymbolId> resolvedType = std::nullopt;
};

// ---- Expression helpers ----

inline std::optional<SymbolId> getExprType(const ExprPtr &expr) {
  if (!expr)
    return std::nullopt;
  return expr->resolvedType;
}

inline void setExprType(const ExprPtr &expr, SymbolId type) {
  if (!expr)
    return;
  expr->resolvedType = type;
}

// ---- Statement types ----

struct DeclStmt : public AstNode {

  inline DeclStmt(const core::SourceLocation &location, const Identifier &name,
                  const TypeName &declType, bool isConst,
                  const ExprPtr &assignedExpr)
      : AstNode(location), identifier(name), type(declType),
        isConstant(isConst), assignedExpression(assignedExpr) {}
  const bool isConstant;
  Identifier identifier;
  const TypeName type;
  const ExprPtr assignedExpression;
};

struct ReturnStmt : public AstNode {
  inline ReturnStmt(const core::SourceLocation &location,
                    const std::optional<ExprPtr> &expr)
      : AstNode(location), expression(expr) {}

  const std::optional<ExprPtr> expression;
};

struct IfStmt : public AstNode {
  inline IfStmt(const core::SourceLocation &location, const ExprPtr &cond,
                const StmtPtr &conseq, const std::optional<StmtPtr> &alt)
      : AstNode(location), condition(cond), consequent(conseq),
        alternative(alt) {}
  const ExprPtr condition;
  const StmtPtr consequent;
  const std::optional<StmtPtr> alternative;
};

struct WhileStmt : public AstNode {
  inline WhileStmt(const core::SourceLocation &location, const ExprPtr &cond,
                   const StmtPtr &bod)
      : AstNode(location), condition(cond), body(bod) {}
  const ExprPtr condition;
  const StmtPtr body;
};

struct AssignmentStmt : public AstNode {
  inline AssignmentStmt(const core::SourceLocation &location,
                        const Identifier &id, const ExprPtr &value)
      : AstNode(location), identifier(id), value(value) {}
  Identifier identifier;
  ExprPtr value;
};

struct Statement : public std::variant<DeclStmt, ReturnStmt, IfStmt, WhileStmt,
                                       ExprPtr, BlockPtr, AssignmentStmt> {
  using Variant = std::variant<DeclStmt, ReturnStmt, IfStmt, WhileStmt, ExprPtr,
                               BlockPtr, AssignmentStmt>;

  template <typename T, typename = std::enable_if_t<
                            !std::is_same_v<std::decay_t<T>, Statement> &&
                            std::is_constructible_v<Variant, T &&>>>
  Statement(const core::SourceLocation &loc, T &&val)
      : Variant(std::forward<T>(val)), location(loc) {}

  const core::SourceLocation location;
};

// ---- Block, Function, Root ----

struct Block : public AstNode {
  inline Block(const core::SourceLocation &location) : AstNode(location) {}
  std::vector<Statement> statements;
};

struct FunctionSpecifications {
  std::vector<ExprPtr> pre, post;
  /// The identifier introduced by `post(r) { ... }`, bound to the return value.
  std::optional<Identifier> postResultBinding;
};

struct Function : public AstNode {
  inline Function(const core::SourceLocation &location, const Identifier &name,
                  const std::vector<Param> &params,
                  const std::optional<TypeName> &returnType, const Block &bod,
                  const FunctionSpecifications &spec)
      : AstNode(location), identifier(name), parameters(params),
        returnType(returnType), body(bod), specifications(spec) {}

  bool returnsVoid() const;

  Identifier identifier;
  const std::vector<Param> parameters;
  const std::optional<TypeName> returnType;
  const FunctionSpecifications specifications;
  const Block body;
};

struct Root : public AstNode {
  inline Root(const core::SourceLocation &location) : AstNode(location) {}
  std::vector<Function> functions;
};

} // namespace blaze::frontend
