#include <iostream>

extern "C" {
  int hello_world();
}

int hello_world() {
    std::cout << "Hello World";
}
