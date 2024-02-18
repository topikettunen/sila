#ifndef SILA_AST_ASTNODE_H
#define SILA_AST_ASTNODE_H

#include "llvm/ADT/PointerUnion.h"

#include "sila/AST/Expression.h"
#include "sila/AST/Statement.h"

namespace sila {

struct ASTNode : public llvm::PointerUnion<Expression *, Statement *> {};

} // namespace sila

#endif // SILA_AST_ASTNODE_H
