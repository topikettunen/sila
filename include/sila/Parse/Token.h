#ifndef SILA_PARSE_TOKEN_H
#define SILA_PARSE_TOKEN_H

#include <cstdint>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"

#include "sila/Basic/SourceLocation.h"

namespace sila {

enum class TokenKind : std::uint8_t {
  Plus,         // +
  PlusEq,       // +=
  Increment,    // ++
  Minus,        // -
  MinusEq,      // -=
  Decrement,    // --
  Asterisk,     // *
  AsteriskEq,   // *=
  Slash,        // /
  SlashEq,      // /=
  Greater,      // >
  GreaterEq,    // >=
  RightShift,   // >>
  RightShiftEq, // >>=
  Lesser,       // <
  LesserEq,     // <=
  LeftShift,    // <<
  LeftShiftEq,  // <<=
  Walrus,       // :=
  Assign,       // =
  Equal,        // ==
  NotEqual,     // <>
  Pipe,         // |
  PipeEq,       // |=
  Ampersand,    // &
  AmpersandEq,  // &=
  LeftBrace,    // {
  RightBrace,   // }
  LeftParen,    // (
  RightParen,   // )
  LeftBracket,  // [
  RightBracket, // ]
  Colon,        // :
  Semicolon,    // ;
  Comma,        // ,
  Spaceship,    // =>
  BinaryLiteral,
  HexaLiteral,
  FloatLiteral,
  DecimalLiteral,
  StringLiteral,
  CharLiteral,
  Keyword,
  Identifier,
  None = 255
};

inline llvm::StringRef getTokenKindText(TokenKind k) {
  switch (k) {
  case TokenKind::Plus:
    return "Plus";
  case TokenKind::PlusEq:
    return "PlusEq";
  case TokenKind::Increment:
    return "Increment";
  case TokenKind::Minus:
    return "Minus";
  case TokenKind::MinusEq:
    return "MinusEq";
  case TokenKind::Decrement:
    return "Decrement";
  case TokenKind::Asterisk:
    return "Asterisk";
  case TokenKind::AsteriskEq:
    return "AsteriskEq";
  case TokenKind::Slash:
    return "Slash";
  case TokenKind::SlashEq:
    return "SlashEq";
  case TokenKind::Greater:
    return "Greater";
  case TokenKind::GreaterEq:
    return "GreaterEq";
  case TokenKind::RightShift:
    return "RightShift";
  case TokenKind::RightShiftEq:
    return "RightShiftEq";
  case TokenKind::Lesser:
    return "Lesser";
  case TokenKind::LesserEq:
    return "LesserEq";
  case TokenKind::LeftShift:
    return "LeftShift";
  case TokenKind::LeftShiftEq:
    return "LeftShiftEq";
  case TokenKind::Walrus:
    return "Walrus";
  case TokenKind::Assign:
    return "Assign";
  case TokenKind::Equal:
    return "Equal";
  case TokenKind::NotEqual:
    return "NotEqual";
  case TokenKind::Pipe:
    return "Pipe";
  case TokenKind::PipeEq:
    return "PipeEq";
  case TokenKind::Ampersand:
    return "Ampersand";
  case TokenKind::AmpersandEq:
    return "AmpersandEq";
  case TokenKind::LeftBrace:
    return "LeftBrace";
  case TokenKind::RightBrace:
    return "RightBrace";
  case TokenKind::LeftParen:
    return "LeftParen";
  case TokenKind::RightParen:
    return "RightParen";
  case TokenKind::LeftBracket:
    return "LeftBracket";
  case TokenKind::RightBracket:
    return "RightBracket";
  case TokenKind::Colon:
    return "Colon";
  case TokenKind::Semicolon:
    return "Semicolon";
  case TokenKind::Comma:
    return "Comma";
  case TokenKind::Spaceship:
    return "Spaceship";
  case TokenKind::BinaryLiteral:
    return "BinaryLiteral";
  case TokenKind::HexaLiteral:
    return "HexaLiteral";
  case TokenKind::FloatLiteral:
    return "FloatLiteral";
  case TokenKind::StringLiteral:
    return "StringLiteral";
  case TokenKind::CharLiteral:
    return "CharLiteral";
  case TokenKind::DecimalLiteral:
    return "DecimalLiteral";
  case TokenKind::Keyword:
    return "Keyword";
  case TokenKind::Identifier:
    return "Identifier";
  case TokenKind::None:
    return "None";
  };

  llvm_unreachable("Unhandled case in switch!");
}

struct Token {
  llvm::StringRef Text;
  SourceLocation Location;
  TokenKind Kind;

  Token() {}

  Token(llvm::StringRef t, SourceLocation l, TokenKind k)
      : Text(t), Location(l), Kind(k) {}
};

} // namespace sila

#endif // SILA_PARSE_TOKEN_H
