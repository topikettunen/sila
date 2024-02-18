#ifndef SILA_AST_EXPRESSION_H
#define SILA_AST_EXPRESSION_H

#include <ostream>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"

#include "sila/Basic/SourceLocation.h"

namespace sila {

enum class ExprKind : std::uint8_t { BinaryLiteral, None = 255 };

inline llvm::StringRef getExprKindText(ExprKind k) {
  switch (k) {
  case ExprKind::BinaryLiteral:
    return "BinaryLiteral";
  case ExprKind::None:
    return "None";
  };

  llvm_unreachable("Unhandled case in switch!");
}

/// Expression - base class for all expressions.
class Expression {
private:
  // Suppress copy in the base class.
  Expression(const Expression &) = delete;
  Expression &operator=(const Expression &) = delete;

protected:
  ExprKind Kind;

public:
  Expression(ExprKind kind) : Kind(kind) {}

  /// Expression destructor set as virtual since this is meant for polymorphic
  /// use.
  virtual ~Expression() noexcept = default;

  /// getKind - return the kind of this expression.
  ExprKind getKind() const { return Kind; }
};

/// NumberLiteralExpr is an abstract base class for all numeric literals.
class NumberLiteralExpr : public Expression {
protected:
  /// Value of the literal.
  llvm::StringRef Value; // Use StringRef instead of APInt or APFloat, which
                         // leak.
  SourceLocation Location;

public:
  NumberLiteralExpr(ExprKind kind, llvm::StringRef value, SourceLocation loc)
      : Expression(kind), Value(value), Location(loc) {}
};

/// BinaryLiteralExpr - Binary literal, like '0b10'.
class BinaryLiteralExpr : public NumberLiteralExpr {
public:
  BinaryLiteralExpr(llvm::StringRef value, SourceLocation loc)
      : NumberLiteralExpr(ExprKind::BinaryLiteral, value, loc) {}

  void debugPrint(std::ostream &o) {
    o << "--- Parsed expr ---\n"
      << Location.toString() << ' ' << std::string(getExprKindText(Kind)) << ' '
      << std::string(Value);
    ;
  }
};

} // namespace sila

#endif // SILA_AST_EXPRESSION_H
