use crate::core::bus::{Bus, BusOp, Device};
use crate::core::cpu::CPU;
use crate::core::variants::{Decoder, Quirks};
use crate::devices::{DMAController, MemoryDevice};
use crate::driver::{SystemInterface, SystemSnapshot};
use crate::shared::*;

enum Phase {
    Phi1,
    Phi2,
}

pub struct GenericSystem<V>
where
    V: Decoder + Quirks,
{
    pub cpu: CPU<V>,
    pub bus: Bus,
    pub dmas: Vec<SharedDevice<dyn DMAController>>,
    phase: Phase,
}

impl<V: Decoder + Quirks> GenericSystem<V> {
    pub fn new(variant: V) -> GenericSystem<V> {
        Self {
            cpu: CPU::new(variant),
            bus: Bus::new(),
            dmas: Vec::new(),
            phase: Phase::Phi1,
        }
    }

    pub fn attach_dma<D>(&mut self, dma: D, base_addr: Word, size: usize)
    where
        D: Device + DMAController + 'static,
    {
        assert!(size.is_power_of_two(), "Device size must be a power of two");
        assert!(size <= (0xFFFF + 1), "Device size exceeds address space");

        let dma_shared = dma.into_shared();
        self.dmas.push(dma_shared.clone());

        let mask = !((size - 1) as Word);
        self.bus.attach_shared_device(&dma_shared, base_addr, mask);
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
        self.phi1();
        self.phi2();
        self.bus.last_op
    }

    fn phi1(&mut self) {
        self.bus.tick();
        self.cpu.phi1();
    }

    fn phi2(&mut self) {
        self.cpu.phi2(&mut self.bus);

        if !self.bus.rdy {
            self.dmas
                .iter()
                .filter(|dma| dma.borrow().wants_bus())
                .for_each(|dma| dma.borrow_mut().dma_tick(&mut self.bus));
        }
    }
}

impl<V: Decoder + Quirks> SystemInterface for GenericSystem<V> {
    fn tick(&mut self) {
        match self.phase {
            Phase::Phi1 => {
                self.phi1();
                self.phi2();
            }

            Phase::Phi2 => {
                self.phi2();
            }
        }

        self.phase = Phase::Phi1;
    }

    fn half_tick(&mut self) {
        match self.phase {
            Phase::Phi1 => {
                self.phi1();
                self.phase = Phase::Phi2;
            }

            Phase::Phi2 => {
                self.phi2();
                self.phase = Phase::Phi1;
            }
        }
    }

    fn bus_as_mut(&mut self) -> &mut Bus {
        &mut self.bus
    }

    fn snapshot(&self) -> SystemSnapshot {
        let cpu = &self.cpu.core;
        SystemSnapshot {
            pc: cpu.pc,
            sp: cpu.sp.value,
            a: cpu.a,
            x: cpu.x,
            y: cpu.y,
            addr_bus: cpu.addr_bus,
            data_bus: cpu.data_bus,
            rw: cpu.rw,
            flags: cpu.flags,
            state: cpu.state,
            instr_name: cpu.instr.name.clone(),
            ir: cpu.ir,
            signals: cpu.signals,
            last_op: self.bus.last_op,
        }
    }
}
