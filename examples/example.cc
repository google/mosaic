//export module example;
//
//export namespace std {
//  inline namespace __1 {
//    template<class _Tp>     class allocator;
//    template <class _Tp, class _Allocator = allocator<_Tp> >
//    class vector;
//  }
//}

//import std.vector;
#include <vector>

struct Example {
  int a, b;
  double c, d;
  //std::vector<int> e;

  [[rust::name("foo")]]
  void foo(int);
  [[rust::name("foo_f64")]]
  void foo(double);
};

int bar(int);
using ::bar;
