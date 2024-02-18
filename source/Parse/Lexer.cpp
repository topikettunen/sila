#include <cctype>
#include <cstdio>
#include <cstdlib>

#include "sila/Parse/Lexer.h"
#include "sila/Parse/Token.h"

namespace sila {

void Lexer::lexLine(const std::size_t lineno, llvm::StringRef line,
                    std::vector<Token> &tokens) {
  std::size_t column = 0;

  // Tokenization helpers.

  auto peek = [&](int n) {
    return (column + n < line.size() && column + n >= 0) ? line[column + n]
                                                         : '\0';
  };

  auto lookupKeyword = [](llvm::StringRef s) -> std::optional<llvm::StringRef> {
    // clang-format off
    static const std::vector<llvm::StringRef> keywords = {
        "return",
        "if", "else",
        "for", "loop", "break",
        "not", "and", "or"
    };
    // clang-format on

    for (const auto &kw : keywords) {
      if (kw == s) {
        return kw;
      }
    }

    return std::nullopt;
  };

  auto storeToken = [&](int len, TokenKind kind) {
    tokens.emplace_back(line.substr(column, len),
                        SourceLocation(lineno, column), kind);
    column += len;
  };

  auto isBinDigit = [](char c) { return c == '0' || c == '1'; };

  // Lexer main loop

  while (column < line.size()) {
    auto peek1 = peek(1);
    auto peek2 = peek(2);

    switch (line[column]) {
      // '<<=' '<<' '<=' '<' '<>'
    case '<':
      if (peek1 == '<') {
        if (peek2 == '=') {
          storeToken(3, TokenKind::LeftShiftEq);
        } else {
          storeToken(2, TokenKind::LeftShift);
        }
      } else if (peek1 == '=') {
        storeToken(2, TokenKind::LesserEq);
      } else if (peek1 == '>') {
        storeToken(2, TokenKind::NotEqual);
      } else {
        storeToken(1, TokenKind::Lesser);
      }
      continue;

      // '>>=' '>>' '>=' '>'
    case '>':
      if (peek1 == '>') {
        if (peek2 == '=') {
          storeToken(3, TokenKind::RightShiftEq);
        } else {
          storeToken(2, TokenKind::RightShift);
        }
      } else if (peek1 == '=') {
        storeToken(2, TokenKind::GreaterEq);
      } else {
        storeToken(1, TokenKind::Greater);
      }
      continue;

      // '++' '+=' '+'
    case '+':
      if (peek1 == '+') {
        storeToken(2, TokenKind::Increment);
      } else if (peek1 == '=') {
        storeToken(2, TokenKind::PlusEq);
      } else {
        storeToken(1, TokenKind::Plus);
      }
      continue;

      //  '--' '-=' '-'
    case '-':
      if (peek1 == '-') {
        storeToken(2, TokenKind::Decrement);
      } else if (peek1 == '=') {
        storeToken(2, TokenKind::MinusEq);
      } else {
        storeToken(1, TokenKind::Minus);
      }
      continue;

      // '|' '|='
    case '|':
      if (peek1 == '=') {
        storeToken(2, TokenKind::PipeEq);
      } else {
        storeToken(1, TokenKind::Pipe);
      }
      continue;

      // '&' '&='
    case '&':
      if (peek1 == '=') {
        storeToken(2, TokenKind::AmpersandEq);
      } else {
        storeToken(1, TokenKind::Ampersand);
      }
      continue;

      // '=' '=='
    case '=':
      if (peek1 == '=') {
        storeToken(2, TokenKind::Equal);
      } else {
        storeToken(1, TokenKind::Assign);
      }
      continue;

    case ':':
      if (peek1 == '=') {
        storeToken(2, TokenKind::Walrus);
      } else {
        storeToken(1, TokenKind::Colon);
      }
      continue;

    case '{':
      storeToken(1, TokenKind::LeftBrace);
      continue;

    case '}':
      storeToken(1, TokenKind::RightBrace);
      continue;

    case '(':
      storeToken(1, TokenKind::LeftParen);
      continue;

    case ')':
      storeToken(1, TokenKind::RightParen);
      continue;

    case '[':
      storeToken(1, TokenKind::LeftBracket);
      continue;

    case ']':
      storeToken(1, TokenKind::RightBracket);
      continue;

    case ';':
      storeToken(1, TokenKind::Semicolon);
      continue;

    case ',':
      storeToken(1, TokenKind::Comma);
      continue;

      // Binary and hexadecimal
    case '0': {
      auto len = 3;

      if (peek1 == 'b') {
        if (isBinDigit(peek2)) {
          while (isBinDigit(peek(len)))
            ++len;

          storeToken(len, TokenKind::BinaryLiteral);
          continue;
        } else {
          Errors.emplace_back(SourceLocation(lineno, column),
                              "Binary literal cannot be empty (0b must be "
                              "followed by binary digits)");
          ++column;
        }
      } else if (peek1 == 'x') {
        if (std::isxdigit(peek2)) {
          while (std::isxdigit(peek(len)))
            ++len;

          storeToken(len, TokenKind::HexaLiteral);
          continue;
        } else {
          Errors.emplace_back(SourceLocation(lineno, column),
                              "Hexadecimal literal cannot be empty (0x must be "
                              "followed by hexadecimal digits)");
          ++column;
        }
      }
    }
      // We let 0 to fallthrough here so we can parse the rest of the number
      // below where other numeric literals will be parsed.
      [[fallthrough]];

    default:
      // Skip any whitespace
      if (std::isspace(line[column])) {
        auto len = 1;

        while (std::isspace(peek(len)))
          ++len;

        // j is the amount of whitespaces found, move column to the next
        // non-whitespace column.
        column += len;

        continue;
      }

      // Number.
      if (std::isdigit(line[column])) {
        auto len = 1;

        // Find the end of the number token in the given line.
        while (std::isdigit(peek(len)))
          ++len;

        // If dot is found in the number, it'll be float. Slurp everything after
        // the dot.
        if (peek(len) == '.') {
          ++len;

          if (!std::isdigit(peek(len)))
            Errors.emplace_back(SourceLocation(lineno, column),
                                "Floating point literal must have at least one "
                                "digit after the decimal point (can be '.0')");

          while (std::isdigit(peek(len)))
            ++len;

          storeToken(len, TokenKind::FloatLiteral);
          continue;
        } else {
          storeToken(len, TokenKind::DecimalLiteral);
          continue;
        }
      }

      // Identifier or keyword
      if (std::isalpha(line[column])) {
        auto len = 1;

        // Find the end of the identifier or keyword.
        while (std::isalnum(peek(len)))
          ++len;

        auto tokVal = line.substr(column, len);
        auto kw = lookupKeyword(tokVal);

        Token tok;

        if (kw != std::nullopt)
          tok = {kw.value(), SourceLocation(lineno, column),
                 TokenKind::Keyword};
        else
          tok = {tokVal, SourceLocation(lineno, column), TokenKind::Identifier};

        tokens.emplace_back(tok);

        // Identifier or keyword is now saved, move the column to the end of the
        // saved token.
        column += len;

        continue;
      }
    }
  }
}

void Lexer::lex(std::string &line) {
  auto lineno = 1;
  std::vector<Token> tokens;

  lexLine(lineno, line, tokens);

  // TODO(tok): Currently there is only one line to be parsed. Obviously, change
  // this.

  TokenMap[lineno] = tokens;
}

}; // namespace sila
