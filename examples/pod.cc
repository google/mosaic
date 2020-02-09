struct Pod {
  int a, b;
  double c, d;
  char e;
};

namespace rust_export {
  using ::Pod;
}
