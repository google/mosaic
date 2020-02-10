struct Pod {
  int a, b;
  char e;
  double c, d;
};

namespace rust_export {
  using ::Pod;
}
