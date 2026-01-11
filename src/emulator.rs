use crate::bus::Bus;
use crate::cpu::CPU;
use crate::variants::Decoder;

pub struct Emulator<V>
where
    V: Decoder,
{
    pub cpu: CPU<V>,
    pub bus: Bus,
    pub cycles: u8,
}

impl<V: Decoder> Emulator<V> {
    pub fn new(variant: V) -> Emulator<V> {
        Self {
            cpu: CPU::new(variant),
            bus: Bus::new(),
            cycles: 0,
        }
    }

    pub fn reset_cpu(&mut self) {
        self.cpu.reset();

        while !self.cpu.core.ready {
            self.tick();
        }
    }

    pub fn tick(&mut self) {
        let _bus_op = self.cpu.tick(&mut self.bus);
        self.cycles += 1;
    }
}
