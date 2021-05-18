// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

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
