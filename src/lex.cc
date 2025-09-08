#include <iostream>

#include <cctype>
#include <ostream>
#include <string>

#include <llvm/Support/ErrorHandling.h>

#include "sila/lex.h"
#include "sila/token.h"

namespace sila {

Lexer::Lexer() {
  // Sort punctuators in descending order.
  sortPuncts();
}

void Lexer::lex(std::string source) {
  SrcPtr = source.c_str();
  CurPtr = SrcPtr;

  // TODO: Working without files for now.
  Location = {.FileName = "<stdio>"};

  while (*CurPtr) {
    // Ignore whitespace
    if (std::isspace(*CurPtr)) {
      ++CurPtr;
      continue;
    }

    // Numeric literal
    if (std::isdigit(*CurPtr)) {
      lexNumber();
      continue;
    }

    // Identifier or keyword
    if (std::isalpha(*CurPtr) || *CurPtr == '_') {
      lexIdent();
      continue;
    }

    // String literal
    if (*CurPtr == '"') {
      lexStringLiteral();
      continue;
    }

    // Char literal
    if (*CurPtr == '\'') {
      lexCharLiteral();
      continue;
    }

    // Punctuator
    if (std::ispunct(*CurPtr)) {
      lexPunct();
      continue;
    }

    llvm_unreachable("unknown character in input");
  }
}

void Lexer::lexNumber() {
  const char* startPtr = CurPtr;
  const char* endPtr = CurPtr;

  // Integer part
  while (std::isdigit(*endPtr))
    ++endPtr;

  bool isFloat = false;

  // Fractional part
  if (*endPtr == '.' && std::isdigit(*(endPtr + 1))) {
    isFloat = true;

    // consume '.'
    ++endPtr;

    while (std::isdigit(*endPtr))
      ++endPtr;
  }

  // Exponent part
  if (*endPtr == 'e' || *endPtr == 'E') {
    const char *expPtr = endPtr + 1;

    if (*expPtr == '+' || *expPtr == '-')
      ++expPtr;

    if (std::isdigit(*expPtr)) {
      isFloat = true;
      endPtr = expPtr + 1;

      while (std::isdigit(*endPtr))
        ++endPtr;
    }
  }

  std::string_view literal(startPtr, endPtr - startPtr);

  TokenKind kind = isFloat ? TokenKind::floating_literal
                           : TokenKind::integer_literal;

  Token tok = newToken(kind,
                       startPtr,
                       static_cast<u32>(endPtr - startPtr),
                       literal);

  Tokens.push_back(tok);
  CurPtr = endPtr;
}

void Lexer::lexPunct() {
  // Try to match longest punctuator at CurPtr
  for (auto const& punct : getPuncts()) {
    size_t len = std::strlen(punct.Text);

    if (std::strncmp(CurPtr, punct.Text, len) == 0) {
      Token tok = newToken(punct.Kind, CurPtr, static_cast<u32>(len), punct.Text);
      Tokens.push_back(tok);

      // Advance pointer
      CurPtr += len;

      return;
    }
  }

  llvm_unreachable("unhandled punctuator");
}

void Lexer::lexIdent() {
  const char* startPtr = CurPtr;
  const char* endPtr = CurPtr;

  while (std::isalnum(*endPtr) || *endPtr == '_')
    ++endPtr;

  std::string_view text(startPtr, endPtr - startPtr);

  TokenKind kind = TokenKind::identifier;

  auto keywords = getKeywords();
  auto it = keywords.find(text);
  if (it != keywords.end()) {
    kind = it->second;
  }

  Token tok = newToken(kind,
                       startPtr,
                       static_cast<u32>(endPtr - startPtr),
                       text);

  Tokens.push_back(tok);
  CurPtr = endPtr;
}

void Lexer::lexStringLiteral() {
  const char* startPtr = CurPtr++;
  const char* endPtr = CurPtr;

  while (*endPtr && *endPtr != '"') {
    if (*endPtr == '\\' && *(endPtr + 1)) {
      endPtr += 2; // skip escaped char
    } else {
      ++endPtr;
    }
  }

  if (*endPtr != '"') {
    llvm_unreachable("unterminated string literal");
  }

  ++endPtr; // consume closing quote

  std::string_view text(startPtr, endPtr - startPtr);

  Token tok = newToken(TokenKind::string_literal,
                       startPtr,
                       static_cast<u32>(endPtr - startPtr),
                       text);

  Tokens.push_back(tok);
  CurPtr = endPtr;
}

void Lexer::lexCharLiteral() {
  const char* startPtr = CurPtr++;
  const char* endPtr = CurPtr;

  if (*endPtr == '\\' && *(endPtr + 1)) {
    endPtr += 2; // escape sequence
  } else {
    ++endPtr;
  }

  if (*endPtr != '\'') {
    // TODO: do some proper error handling here.
    llvm_unreachable("unterminated char literal");
  }

  ++endPtr; // consume closing '

  std::string_view text(startPtr, endPtr - startPtr);

  Token tok = newToken(TokenKind::char_literal,
                       startPtr,
                       static_cast<u32>(endPtr - startPtr),
                       text);

  Tokens.push_back(tok);
  CurPtr = endPtr;
}

void Lexer::lexClear() {
  Location = SourceLocation{};
  Tokens.clear();
}

void Lexer::debugPrint(std::ostream &o) {
  o << "--- Tokens length " << Tokens.size() << "\n";
  o << "--- Section starting at line " << Location.Line << "\n";
  for (auto const &token : Tokens) {
    o << "    " << token.Text << " (len=" << token.Length << ") "
      << getTokenText(token.Kind) << "\n";
  }
}

} // namespace sila
