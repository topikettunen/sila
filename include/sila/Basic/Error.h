#ifndef SILA_BASIC_ERROR_H
#define SILA_BASIC_ERROR_H

#include "sila/Basic/SourceLocation.h"

namespace sila {

struct ErrorEntry {
  SourceLocation where;
  std::string what;

  ErrorEntry(SourceLocation where, std::string what)
      : where{where}, what{what} {}

  bool operator==(ErrorEntry const &that) {
    return where == that.where && what == that.what;
  }
};

} // namespace sila

#endif // SILA_BASIC_ERROR_H
