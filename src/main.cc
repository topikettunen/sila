#include <cstdlib>
#include <iostream>
#include <string>

#include "sila/lex.h"
#include "sila/token.h"

namespace {

int silaRepl() {
  sila::Lexer lex;

  std::string line;
  while (true) {
    std::cout << ">>> ";

    if (!std::getline(std::cin, line)) {
      // handle EOF (Ctrl+d, Ctrl+z)
      break;
    }

    if (line == "quit" || line == "exit")
      break;

    lex.lex(line);

    lex.debugPrint(std::cout);

    // Flush output
    std::cout << std::endl;

    // TODO: For now, work with single lines only.
    lex.lexClear();
  }

  return EXIT_SUCCESS;
}

} // namespace

int main() { return silaRepl(); }
