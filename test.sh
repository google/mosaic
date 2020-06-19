#!/bin/sh -e

cargo test -- "$@"
cargo run --bin=runtests -- "$@"
