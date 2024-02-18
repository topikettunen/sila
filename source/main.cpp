#include <cstdlib>
#include <iostream>

#include "sila/Driver/Driver.h"

int main() {
  sila::Driver d;
  d.run(std::cin);
  return EXIT_SUCCESS;
}
