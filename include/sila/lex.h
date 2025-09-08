#pragma once

#include <vector>

#include "sila/common.h"
#include "sila/macros.h"
#include "sila/token.h"

namespace sila {

struct Lexer {
  char const *SrcPtr = nullptr;
  char const *CurPtr = nullptr;

  SourceLocation Location;
  std::vector<Token> Tokens;

  Lexer();
  SILA_NONCOPYABLE_NONMOVABLE(Lexer)
  ~Lexer() = default;

  void lex(std::string source);

  void lexNumber();
  void lexPunct();
  void lexIdent();
  void lexStringLiteral();
  void lexCharLiteral();

  void lexClear();

  void debugPrint(std::ostream &o);
};

} // namespace sila
