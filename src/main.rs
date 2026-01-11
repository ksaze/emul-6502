#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use crate::{bus::RAM64K, variants::NMOS_6502};

mod bus;
mod cpu;
mod emulator;
mod operations;
mod shared;
mod variants;

fn main() {
    let mut emul = emulator::Emulator::new(NMOS_6502);
    emul.bus.attach_device(RAM64K::new());

    emul.cpu.core.a = 0x42;
    emul.bus.write(0x0000, 0x85); // LDA immediate opcode
    emul.bus.write(0x0001, 0x10);
    emul.reset_cpu();

    emul.tick();
    emul.tick();
    emul.tick();

    assert_eq!(emul.bus.read(0x0010), 0x42);
    assert_eq!(emul.cpu.core.pc, 0x2);
    println!("Cycles: {}", emul.cycles);
    println!("Execution completed.");
}
