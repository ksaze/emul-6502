#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use mos65x::core::bus::BusOp;
use mos65x::core::variants::NMOS_6502;
use mos65x::emulator::Emulator;

mod core;
mod emulator;
mod shared;

fn main() {
    let mut emul = Emulator::new(NMOS_6502);
    // Full RAM
    emul.attach_ram(0x0000, 0x10000);
    emul.bus.write_raw(0x0, 0x58);
    emul.bus.write_raw(0x1, 0xE8);
    emul.bus.write_raw(0x2, 0xD0);
    emul.bus.write_raw(0x3, 0xFE);

    for _ in 0..14 {
        let op = emul.tick();
        match op {
            BusOp::Read(addr, data) => {
                println!("read {:#4X} {:#4X}", addr, data);
            }
            BusOp::Write(addr, data) => {
                println!("write {:#4X} {:#4X}", addr, data);
            }
            BusOp::Internal => {
                println!("Internal");
            }
        }
    }
    print!("{}", emul.cpu.core.instr.name);
}
