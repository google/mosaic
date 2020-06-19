extern crate pod_bind;
use pod_bind::Pod;

fn main() {
    let mut pod = Pod {
        a: 1,
        b: 2,
        c: 0.1,
        d: 10.0,
        e: '!' as i8,
    };
    pod.foo(42);
}
