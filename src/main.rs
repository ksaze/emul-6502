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

    // Attach full RAM
    emul.attach_ram(0x0000, 0x10000);

    // Reset vector → start at $0000
    emul.bus.write(0xFFFC, 0x00);
    emul.bus.write(0xFFFD, 0x00);

    // Program at $0000:
    // JMP ($0010)
    emul.bus.write(0x0000, 0x6C); // JMP (abs)
    emul.bus.write(0x0001, 0x10); // pointer low
    emul.bus.write(0x0002, 0x00); // pointer high

    // Indirect pointer at $0010 → $1234
    emul.bus.write(0x0010, 0x34); // target low
    emul.bus.write(0x0011, 0x12); // target high

    // Reset CPU (loads PC from reset vector)
    emul.reset_cpu();

    // Run enough cycles for JMP (abs)
    // NMOS6502 takes 5 cycles
    for _ in 0..5 {
        emul.tick();
    }

    // Verify jump
    assert_eq!(emul.cpu.core.pc, 0x1234);

    println!("Indirect JMP test passed");
    println!("Cycles executed: {}", emul.cycles);
}
