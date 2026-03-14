use crate::core::bus::{Bus, BusOp, MemoryDevice};
use crate::core::cpu::CPU;
use crate::core::variants::{Decoder, Quirks};
use crate::shared::*;

pub struct Emulator<V>
where
    V: Decoder + Quirks,
{
    pub cpu: CPU<V>,
    pub bus: Bus,
    pub cycles: u64,
}

impl<V: Decoder + Quirks> Emulator<V> {
    pub fn new(variant: V) -> Emulator<V> {
        Self {
            cpu: CPU::new(variant),
            bus: Bus::new(),
            cycles: 0,
        }
    }

    pub fn attach_rom(&mut self, mut rom_data: Vec<Byte>, base_addr: Word) {
        let size = rom_data.len().next_power_of_two();
        assert!(size <= (0xFFFF + 1), "ROM size exceeds address space.");
        rom_data.resize(size, 0xFF);

        assert!(
            (base_addr as usize & (size - 1)) == 0,
            "base address must be aligned to size"
        );

        let mask = !((size - 1) as Word);
        let rom = MemoryDevice::rom(rom_data);
        self.bus.attach_device(rom, base_addr, mask);
    }

    pub fn attach_ram(&mut self, base_addr: Word, size: usize) {
        assert!(size.is_power_of_two(), "RAM size must be a power of two");
        assert!(size <= (0xFFFF + 1), "RAM size exceeds address space");

        let mask = !((size - 1) as Word);
        let ram = MemoryDevice::ram(size);
        self.bus.attach_device(ram, base_addr, mask);
    }

    pub fn tick(&mut self) -> BusOp {
        self.bus.tick();
        self.cpu.phi1();
        self.cpu.phi2(&mut self.bus);
        self.cycles += 1;
        self.bus.last_op
    }
}
