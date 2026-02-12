#include "Ast.h"
bool blaze::frontend::Function::returnsVoid() const {
  if (!returnType.has_value())
    return true;
  return returnType->identifier.name == "void";
}

const char *
blaze::frontend::binaryOperationToString(const BinaryOperation &op) {
  switch (op) {
  case Addition:
    return "+";
  case Subtraction:
    return "-";
  case Multiplication:
    return "*";
  case Division:
    return "/";
  case LessThan:
    return "<";
  case LessEqual:
    return "<=";
  case GreaterThan:
    return ">";
  case GreaterEqual:
    return ">=";
  case Equal:
    return "==";
  case NotEqual:
    return "!=";
  }
}
