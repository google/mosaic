// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate pod_bind;
use pod_bind::export::Pod;

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
