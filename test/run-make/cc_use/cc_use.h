#include <iostream>

struct Foo
{
  int a, b;
  char e;
  double c, d;
  void say_hello(int arg)
  {
    std::cout << "hello from C++. arg: " << arg << ", a: " << a << ", b: " << b << "\n";
  }
};

namespace nested
{
  struct Bar
  {
    int x;

    void say_goodbye(int arg)
    {
      std::cout << "goodbye from C++. arg: " << arg << ", x: " << x << "\n";
    }
  };
} // namespace nested
