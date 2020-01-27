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
}
