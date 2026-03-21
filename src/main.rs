#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]
use mos65x::{core::variants::NMOS_6502, generic_system::GenericSystem};

fn main() {
    let mut system = GenericSystem::new(NMOS_6502);

    // 64KB RAM
    system.attach_ram(0x0000, 0x10000);

    for i in 0..0xFFEF {
        system.bus.write_raw(i, 0xE8);
    }
    system.bus.write_raw(0xFFEF, 0xFF);

    // Reset vector -> $0000
    system.bus.write_raw(0xFFFC, 0x00);
    system.bus.write_raw(0xFFFD, 0x00);
    //     let mut driver = DebugDriver::new(system);
    //     driver.run().unwrap();
}
