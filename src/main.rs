#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]

use crate::bus::BusOp;
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

    for _ in 0..10 {
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
