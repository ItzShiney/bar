#![allow(clippy::unit_arg)]

mod hash_map_insert;
mod trace;
pub mod vm;

use nom::error::convert_error;
pub use {
    hash_map_insert::*,
    trace::*,
    vm::VM,
};

fn main() {
    let mut vm = VM::default();
    let input = include_str!("code.bar");

    if let Err(err) = vm.run(input) {
        println!("{}", convert_error(input, err));
    }
}
