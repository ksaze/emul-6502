use crate::core::{
    Byte, Word,
    bus::{BusMaster, Device},
};

pub trait DMAController {
    fn wants_bus(&mut self) -> bool;
    fn dma_tick(&mut self, bus: &mut dyn BusMaster);
}

pub struct MockDMA {}

impl MockDMA {
    pub fn new() -> Self {
        MockDMA {}
    }
}

impl Device for MockDMA {
    fn read(&mut self, _addr: Word) -> Byte {
        0xFF
    }

    fn write(&mut self, _addr: Word, _val: Byte) {}

    fn tick(&mut self) {}
}

impl DMAController for MockDMA {
    fn wants_bus(&mut self) -> bool {
        true
    }
    fn dma_tick(&mut self, _bus: &mut dyn BusMaster) {}
}

pub struct EmulatorControl {
    pub nmi_line: bool,
    pub irq_line: bool,
    pub res_line: bool,
    pub rdy_line: bool,
}

impl EmulatorControl {
    pub fn new() -> Self {
        Self {
            nmi_line: true,
            irq_line: true,
            res_line: true,
            rdy_line: true,
        }
    }
}

impl Device for EmulatorControl {
    fn read(&mut self, _addr: Word) -> Byte {
        // Garbage Value. Never Triggered
        0xFF
    }
    fn write(&mut self, _addr: Word, _val: Byte) {}
    fn tick(&mut self) {}

    fn nmi(&self) -> bool {
        self.nmi_line
    }
    fn irq(&self) -> bool {
        self.irq_line
    }
    fn res(&self) -> bool {
        self.res_line
    }
    fn rdy(&self) -> bool {
        self.rdy_line
    }
}

pub struct MemoryDevice {
    data: Box<[Byte]>,

    readonly: bool,
}

impl MemoryDevice {
    fn new(data: Box<[Byte]>, readonly: bool) -> Self {
        Self { data, readonly }
    }

    pub fn ram(size: usize) -> Self {
        assert!(size.is_power_of_two());
        Self::new(vec![0; size].into_boxed_slice(), false)
    }

    pub fn rom(rom_data: Vec<Byte>) -> Self {
        Self::new(rom_data.into_boxed_slice(), true)
    }
}

impl Device for MemoryDevice {
    #[inline]
    fn read(&mut self, addr: Word) -> Byte {
        self.data[addr as usize]
    }

    #[inline]
    fn write(&mut self, addr: Word, val: Byte) {
        if self.readonly {
            return;
        }

        self.data[addr as usize] = val;
    }

    fn tick(&mut self) {
        // No timing behavior for memory devices
    }
}
