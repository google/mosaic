// Copyright (c) 2021 Google LLC
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::marker::PhantomData;

// defined in accuse crate
trait Wraps<T> {
    fn from_wrapped(other: T) -> Self;
    fn into_wrapped(self) -> T;
}

trait BridgeFrom<T> {
    fn bridge_from(other: T) -> Self;
}

trait BridgeTo<T> {
    fn bridge(self) -> T;
}
impl<T, U> BridgeTo<U> for T
where
    U: BridgeFrom<T>,
{
    fn bridge(self) -> U {
        U::bridge_from(self)
    }
}

// defined in the "root" crate, my_vec, wrapping our C++ library.
cc_use!(<vector> in libstdc++, std::vector); // or maybe:
cc_bind!(<vector> in libstdc++ as vector, std::vector);
pub use vector::*;
// ^ `in libstdc++` is so we can grab relevant CFLAGS from metadata
// ^ expands to:
// extern crate libstdc_bind_my_vec;
// use libstdc_bind_my_vec::vector;

unsafe trait InstantiatesVector<T> {
    type Repr;
    const INSTANCE_HASH: usize;

    // maybe some methods can go here, if we determine / mark them as universal?
}

// defined in local instantiating crate
cc_use!(my_vec::vector::{<i32>, <bool>});
// ^ expands to:
// use my_vec::InstantiatesVector;  // we can infer it's generic, so we need the trait
// extern crate libstdc_bind_local;
// use libstdc_bind_local::{prelude::*, vector};
use remote::remote;

//  in libstdc_bind_local:
trait HasLocalVectorInstance {
    type Repr;
}
struct LocalVectorReprI32;
#[repr(transparent)]
struct LocalVector<T: HasLocalVectorInstance>(T::Repr);

impl HasLocalVectorInstance for i32 {
    type Repr = LocalVectorReprI32;
}
unsafe impl InstantiatesVector<i32> for LocalVector<i32> {
    type Repr = LocalVectorReprI32;
    const INSTANCE_HASH: usize = 0xdeadbeef;
}

impl<T: InstantiatesVector<U>, U: HasLocalVectorInstance> BridgeFrom<T> for LocalVector<U> {
    fn bridge_from(mut other: T) -> Self {
        // TODO: static assert that INSTANCE_HASH, sizes are equal
        unsafe { std::ptr::read(&mut other as *mut T as *mut Self) }
    }
}

//  in local:
fn local(x: LocalVector<i32>) {
    remote(x.bridge())
}

// defined in remote instantiating crate
cc_use!(my_vec::vector::{<i32>});
trait HasRemoteVectorInstance {
    type Repr;
}
struct RemoteVectorReprI32;
#[repr(transparent)]
struct RemoteVector<T: HasRemoteVectorInstance>(T::Repr);

impl HasRemoteVectorInstance for i32 {
    type Repr = RemoteVectorReprI32;
}
unsafe impl InstantiatesVector<i32> for RemoteVector<i32> {
    type Repr = RemoteVectorReprI32;
    const INSTANCE_HASH: usize = 0xdeadbeef;
}

impl<T: InstantiatesVector<U>, U: HasRemoteVectorInstance> BridgeFrom<T> for RemoteVector<U> {
    fn bridge_from(mut other: T) -> Self {
        // TODO: static assert that INSTANCE_HASH, sizes are equal
        unsafe { std::ptr::read(&mut other as *mut T as *mut Self) }
    }
}

fn remote(_x: RemoteVector<i32>) {}
