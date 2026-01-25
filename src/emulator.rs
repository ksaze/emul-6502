use crate::bus::{Bus, BusOp, MemoryDevice};
use crate::cpu::{CPU, CPUState};
use crate::shared::*;
use crate::variants::{ALUVariant, Decoder};

pub struct Emulator<V>
where
    V: Decoder + ALUVariant,
{
    pub cpu: CPU<V>,
    pub bus: Bus,
    pub cycles: u8,
}

impl<V: Decoder + ALUVariant> Emulator<V> {
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

    pub fn reset_cpu(&mut self) {
        self.cpu.reset();

        while self.cpu.core.state == CPUState::Exec {
            self.tick();
        }
    }

    pub fn tick(&mut self) -> BusOp {
        self.cpu.tick(&mut self.bus);
        let bus_op = self.bus.tick();
        self.cycles += 1;
        bus_op
    }
}
