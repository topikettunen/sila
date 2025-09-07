#pragma once

#include <vector>

#include "sila/Common.h"
#include "sila/Macros.h"
#include "sila/Token.h"

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
