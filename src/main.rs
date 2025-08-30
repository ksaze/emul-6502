#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use crate::{bus::MemoryBus, variants::Nmos6502};

mod bus;
mod cpu;
mod emulator;
mod operations;
mod shared;
mod variants;

fn main() {
    let mut emul = emulator::Emulator::new(Nmos6502::new(), MemoryBus::new());
    emul.reset_cpu();
    println!("Cycles: {}", emul.cycles);
    println!("Execution completed.");
}
