struct Pod {
  int a, b;
  double c, d;
  char e;

  Pod(const Pod&) {}
};

namespace rust_export {
  using ::Pod;
}
