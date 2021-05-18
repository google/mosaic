// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#include <vector>

struct Example {
  int a, b;
  double c, d;
  std::vector<int> e;

  [[rust::name("foo")]]
  void foo(int);
  [[rust::name("foo_f64")]]
  void foo(double);
};

namespace rust_export {
  using ::Example;
  using vector_int = std::vector<int>;
  template<class T>
  using vectorT = std::vector<T>;
  using std::vector;
}
