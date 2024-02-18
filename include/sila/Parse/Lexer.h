#ifndef SILA_PARSE_LEXER_H
#define SILA_PARSE_LEXER_H

#include <map>
#include <ostream>

#include "llvm/ADT/ArrayRef.h"

#include "sila/Basic/Error.h"
#include "sila/Parse/Token.h"

namespace sila {

// TODO(tok): I wonder if there's some container in LLVM for this sort of type?
// For now, using this, since I want to iterate over the collection in sorted
// order.
using TokenMap = std::map<std::size_t, llvm::SmallVector<Token, 0>>;

/// Lexer - TODO(tok): write docs
class Lexer {
private:
  // Suppress copy
  Lexer(const Lexer &) = delete;
  Lexer &operator=(const Lexer &) = delete;

  llvm::ArrayRef<ErrorEntry> Errors;
  TokenMap Tokens;

public:
  Lexer(llvm::ArrayRef<ErrorEntry> errors) : Errors(errors) {}

  void lex(llvm::StringRef line);

  void lexLine(const std::size_t lineno, llvm::StringRef line, TokenMap tokens);

  TokenMap getTokens() { return Tokens; }

  void debugPrint(std::ostream &o) {
    for (const auto &[lineno, tokens] : Tokens) {
      o << "---  Line no. " << lineno << " tokens found: " << tokens.size()
        << " ---\n";

      for (const auto &tok : tokens) {
        o << tok.Location.toString() << ' '
          << std::string(getTokenKindText(tok.Kind)) << ' '
          << std::string(tok.Text)
          << " size: " << std::to_string(tok.Text.size()) << '\n';
      }
    }
  }
};

} // namespace sila

#endif // SILA_PARSE_LEXER_H
