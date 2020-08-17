// TODO: Make paths like Foo::Pod work
extern crate cc_use_bind;
use cc_use::cc_use;
cc_use!("cc_use.h", Foo, nested::Bar);

fn main() {
    let mut foo = Foo {
        a: 1,
        b: 2,
        c: 0.1,
        d: 10.0,
        e: '!' as i8,
    };
    foo.say_hello(42);

    let mut bar = Bar { x: 1337 };
    bar.say_goodbye(84);
}
