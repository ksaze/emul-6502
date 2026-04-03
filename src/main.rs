#![warn(clippy::all, clippy::pedantic)]
#![allow(dead_code, clippy::missing_docs_in_private_items)]
use mos65x::{
    core::variants::NMOS_6502, devices::MockDMA, driver::DebugDriver, generic_system::RcSystem,
};

fn main() {
    let mut system = RcSystem::new(NMOS_6502);
    let dma_dev = MockDMA::new();
    system.attach_dma(dma_dev, 0x0, 0x4);

    // 64KB RAM
    system.attach_ram(0x0000, 0x10000, 1);

    for i in 0..0xFFEF {
        system.bus.write_raw(i, 0xE8);
    }
    system.bus.write_raw(0xFFEF, 0xFF);

    // Reset vector -> $0000
    system.bus.write_raw(0xFFFC, 0x00);
    system.bus.write_raw(0xFFFD, 0x00);

    let mut driver = DebugDriver::new(system);
    driver.run().unwrap();
}
