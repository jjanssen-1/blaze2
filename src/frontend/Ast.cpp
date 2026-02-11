#include "Ast.h"
bool blaze::frontend::Function::returnsVoid() const {
  if (!returnType.has_value())
    return true;
  return returnType->identifier.name == "void";
}
