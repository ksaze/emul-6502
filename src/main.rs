#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use crate::variants::NMOS_6502;

mod bus;
mod cpu;
mod emulator;
mod operations;
mod shared;
mod variants;

fn main() {
    let mut emul = emulator::Emulator::new(NMOS_6502);
    // Full RAM
    emul.attach_ram(0x0000, 0x10000);

    emul.bus.write(0x0, 0x24);
    emul.bus.write(0x1, 0x05);
    emul.bus.write(0x2, 0x03);

    emul.tick();
    print!("{}", emul.cpu.core.instr.name);
}
