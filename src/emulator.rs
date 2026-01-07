use crate::bus::Bus;
use crate::cpu::CPU;
use crate::variants::Decoder;

pub struct Emulator<V, B>
where
    V: Decoder,
    B: Bus,
{
    pub cpu: CPU<V>,
    pub bus: B,
    pub cycles: u8,
}

impl<V: Decoder, B: Bus> Emulator<V, B> {
    pub fn new(variant: V, bus: B) -> Emulator<V, B> {
        Self {
            cpu: CPU::new(variant),
            bus,
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
        self.cpu.tick(&mut self.bus);
        self.cycles += 1;
    }
}
