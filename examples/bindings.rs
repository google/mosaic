#![feature(fn_traits, unboxed_closures)]
// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(dead_code)]
#![allow(non_camel_case_types)]

/// Notes about how (not) to generate bindings, as code.

mod templates {
    struct Vector<T>(std::marker::PhantomData<T>);

    trait VectorMethods<T> {
        fn new() -> Self;
        fn push(&self, item: T);
    }

    impl VectorMethods<i32> for Vector<i32> {
        fn new() -> Self {
            panic!()
        }
        fn push(&self, _item: i32) {
            panic!()
        }
    }

    impl VectorMethods<String> for Vector<String> {
        fn new() -> Self {
            panic!()
        }
        fn push(&self, _item: String) {
            panic!()
        }
    }

    fn do_something<T>(_vec: &impl VectorMethods<T>) {
        panic!()
    }

    fn main() {
        let vec = Vector::new();
        vec.push(0i32);
        do_something(&vec);
    }
}

mod overloading {
    // class Foo {
    // public:
    //   void bar(int32_t) const;
    //   void bar(double); -- TODO make non-const
    // }

    //#[derive(Debug)]
    struct Foo {
        // public methods here
        pub bar: _Foo_bar,

        // public data members here
        pub data: u64,
    }

    struct _Foo_bar(());

    impl FnOnce<(i32,)> for _Foo_bar {
        type Output = ();
        extern "rust-call" fn call_once(self, (_a,): (i32,)) {
            panic!("can't consume method object")
        }
    }
    impl FnMut<(i32,)> for _Foo_bar {
        extern "rust-call" fn call_mut(&mut self, (a,): (i32,)) {
            <Self as Fn<(i32,)>>::call(self, (a,))
        }
    }
    impl Fn<(i32,)> for _Foo_bar {
        extern "rust-call" fn call(&self, (a,): (i32,)) {
            println!("Foo::bar(int): this={:?} a={}", self as *const Self, a);
        }
    }

    impl<'this> FnOnce<(f64,)> for _Foo_bar {
        type Output = ();
        extern "rust-call" fn call_once(self, (_a,): (f64,)) {
            panic!("can't consume method object")
        }
    }
    impl<'this> FnMut<(f64,)> for _Foo_bar {
        extern "rust-call" fn call_mut(&mut self, (a,): (f64,)) {
            <Self as Fn<(f64,)>>::call(self, (a,))
        }
    }
    impl<'this> Fn<(f64,)> for _Foo_bar {
        extern "rust-call" fn call(&self, (a,): (f64,)) {
            println!("Foo::bar(double): this={:?} a={}", self as *const Self, a);
        }
    }

    fn main() {
        let foo = Foo {
            bar: _Foo_bar(()),
            data: 1234,
        };
        println!("Foo lives at {:?}", &foo as *const Foo);
        (foo.bar)(42);
        (foo.bar)(3.141);
    }
}

mod overloading2 {
    struct Foo(i32);

    trait BarU32 {
        fn bar(&self, val: u32);
    }
    impl BarU32 for Foo {
        fn bar(&self, val: u32) {
            println!("BarU32: {} / {}", self.0, val);
        }
    }

    trait BarU64 {
        fn bar(&self, val: u64);
    }
    impl BarU64 for Foo {
        fn bar(&self, val: u64) {
            println!("BarU64: {} / {}", self.0, val);
        }
    }

    pub fn main() {
        let foo = Foo(42);
        // foo.bar(123u64);
        //~^ ERROR: multiplate applicable items in scope
        BarU64::bar(&foo, 42);
    }
}

mod const_overloading {
    struct Foo(i32);

    impl Foo {
        fn bar(&mut self) -> &mut i32 {
            println!("mut");
            &mut self.0
        }
    }

    trait Bar<'a>
    where
        Self: Sized,
    {
        fn bar(self) -> &'a i32;
    }

    impl<'a> Bar<'a> for &'a Foo
    where
        Self: Sized,
    {
        fn bar(self) -> &'a i32 {
            println!("const");
            &self.0
        }
    }

    // This situation is why we should rewrite nonconst versions to include `_mut`.
    fn main() {
        let mut foo = Foo(42);
        println!("{}", foo.bar());
        *(&mut foo).bar() += 1;
        println!("{}", foo.bar());
    }
}

mod exports {
    mod foo {
        mod cpp {
            pub struct Foo {
                pub bar: ns::Bar,
            }
            pub mod ns {
                pub struct Bar;
            }
            pub fn make_bar() -> ns::Bar {
                todo!()
            }
        }

        pub type Foo = cpp::Foo;
        pub use cpp::make_bar;
    }

    fn main() {
        let bar = foo::make_bar();
        let _sz = std::mem::size_of_val(&bar);
        let _foo = foo::Foo { bar };
    }
}

mod this {
    use std::ptr::NonNull;
    struct NonNullConst<T>(NonNull<T>);

    struct Foo(i32);

    trait Stuff {
        fn stuff(self);
    }

    impl Stuff for NonNull<Foo> {
        fn stuff(self) {
            println!("mut");
        }
    }

    impl Foo {
        fn stuff(&mut self) {
            NonNull::from(self).stuff();
        }
    }

    pub fn main() {
        let mut foo = Foo(42);
        foo.stuff();
    }
}

fn main() {
    this::main();
}
