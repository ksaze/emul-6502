#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use crate::{bus::MemoryBus, variants::NMOS_6502};

mod bus;
mod cpu;
mod emulator;
mod operations;
mod shared;
mod variants;

fn main() {
    let mut emul = emulator::Emulator::new(NMOS_6502, MemoryBus::new());
    emul.reset_cpu();
    println!("Cycles: {}", emul.cycles);
    println!("Execution completed.");
}
