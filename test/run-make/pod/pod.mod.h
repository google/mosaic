#include <iostream>

struct Pod
{
  int a, b;
  char e;
  double c, d;
  void foo(int arg)
  {
    std::cout << "hello from C++. arg: " << arg << ", a: " << a << ", b: " << b << "\n";
  }
};

namespace rust_export
{
  using ::Pod;
}
