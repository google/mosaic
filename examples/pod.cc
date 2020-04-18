struct Pod
{
  int a, b;
  char e;
  double c, d;
  void foo(int);
};

namespace rust_export
{
using ::Pod;
}
