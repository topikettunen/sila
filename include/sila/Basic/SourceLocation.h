#ifndef SILA_BASIC_SOURCELOCATION_H
#define SILA_BASIC_SOURCELOCATION_H

#include <cstdlib>
#include <string>

namespace sila {

struct SourceLocation {
  std::size_t line;   // one-based
  std::size_t column; // one-based

  SourceLocation(std::size_t l = 1, std::size_t c = 1) : line{l}, column{c} {}

  auto operator<=>(SourceLocation const &) const = default;

  std::string toString() const {
    return "(" + std::to_string(line) + "," + std::to_string(column) + ")";
  }
};

} // namespace sila

#endif // SILA_BASIC_SOURCELOCATION_H
