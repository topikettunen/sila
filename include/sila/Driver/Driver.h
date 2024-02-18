#ifndef SILA_DRIVER_DRIVER_H
#define SILA_DRIVER_DRIVER_H

#include <istream>

#include "sila/Basic/Error.h"
#include "sila/Parse/Lexer.h"
#include "sila/Parse/Parser.h"

namespace sila {

class Driver {
private:
  llvm::SmallVector<ErrorEntry, 0> Errors;

  Lexer Lexer;
  Parser Parser;

public:
  Driver() : Lexer(Errors), Parser(Errors) {}

  enum class RC { eof, notEOF };

  RC run(std::istream &is);
};

}; // namespace sila

#endif // SILA_DRIVER_DRIVER_H
