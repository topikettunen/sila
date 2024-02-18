#ifndef SILA_PARSE_PARSER_H
#define SILA_PARSE_PARSER_H

#include "llvm/ADT/ArrayRef.h"

#include "sila/Basic/Error.h"
#include "sila/Parse/Lexer.h"

namespace sila {

/// Parser - TODO(tok): write docs
class Parser {
private:
  // Suppress copy
  Parser(const Parser &) = delete;
  Parser &operator=(const Parser &) = delete;

  llvm::ArrayRef<ErrorEntry> Errors;

public:
  Parser(llvm::ArrayRef<ErrorEntry> errors) : Errors(errors) {}

  void parse(llvm::ArrayRef<Token> tokens);
};

} // namespace sila

#endif // SILA_PARSE_PARSER_H
