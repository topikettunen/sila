#include <iostream>

#include "sila/Driver/Driver.h"

namespace sila {

Driver::RC Driver::run(std::istream &is) {
  std::string line;

  // Lex the given line to stdout.
  //
  // TODO(tok): currently only handles one line, fix it eventually.
  while (std::getline(is, line)) {
    Lexer.lex(line);

    Lexer.debugPrint(std::cout);
  }

  for (const auto &[lineno, tokens] : Lexer.getTokens()) {
    for (const auto &tok : tokens) {
      if (tok.Kind == TokenKind::BinaryLiteral)
        // TODO
        ;
    }
  }

  return RC::eof;
}

}; // namespace sila
