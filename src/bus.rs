use crate::shared::{Byte, Word};

pub trait Device {
    fn read(&mut self, addr: Word) -> Byte;
    fn write(&mut self, addr: Word, val: Byte);
    fn tick(&mut self);
}

pub struct MemoryDevice {
    data: Box<[Byte]>,

    readonly: bool,
}

impl MemoryDevice {
    pub fn new(data: Box<[Byte]>, readonly: bool) -> Self {
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

pub struct RAM64K {
    data: [Byte; 0xFFFF + 1],
}

impl RAM64K {
    #[must_use]
    #[allow(clippy::large_stack_arrays)]
    pub fn new() -> Self {
        Self {
            data: [0; (0xFFFF + 1)],
        }
    }
}

impl Device for RAM64K {
    fn read(&mut self, addr: Word) -> Byte {
        self.data[addr as usize]
    }

    fn write(&mut self, addr: Word, val: Byte) {
        self.data[addr as usize] = val;
    }

    fn tick(&mut self) {}
}

pub struct BusMapping {
    pub base: Word,
    pub mask: Word,
    pub device: Box<dyn Device>,
}

impl BusMapping {
    fn maps(&self, addr: Word) -> bool {
        (addr & self.mask) == self.base
    }

    fn offset(&self, addr: Word) -> Word {
        addr & !self.mask
    }
}

#[derive(Clone, Copy)]
pub enum BusOp {
    Read(Word, Byte),
    Write(Word, Byte),
    Internal,
}

pub struct Bus {
    mappings: Vec<BusMapping>,
    last_op: BusOp,
    pub data_bus: Byte,
    pub addr_bus: Word,
    rdy: bool,
}

impl Bus {
    pub fn new() -> Self {
        Self {
            mappings: Vec::new(),
            last_op: BusOp::Internal,
            data_bus: 0x0,
            addr_bus: 0x0,
            rdy: true,
        }
    }

    pub fn attach_device<D: Device + 'static>(&mut self, device: D, base: Word, mask: Word) {
        self.mappings.push(BusMapping {
            base,
            mask,
            device: Box::new(device),
        });
    }

    fn find_device_mut(&mut self, addr: Word) -> Option<&mut BusMapping> {
        self.mappings.iter_mut().find(|map| map.maps(addr))
    }

    pub fn read(&mut self, addr: Word) -> Byte {
        if let Some(map) = self.find_device_mut(addr) {
            let val = map.device.read(map.offset(addr));
            self.data_bus = val;
            self.last_op = BusOp::Read(addr, val);
            self.addr_bus = addr;
            val
        } else {
            self.last_op = BusOp::Read(addr, self.data_bus);
            self.data_bus
        }
    }

    pub fn write(&mut self, addr: Word, val: Byte) {
        self.last_op = BusOp::Write(addr, val);
        self.data_bus = val;
        self.addr_bus = addr;

        if let Some(map) = self.find_device_mut(addr) {
            map.device.write(map.offset(addr), val);
        }
    }

    pub fn tick(&mut self) -> BusOp {
        for map in self.mappings.iter_mut() {
            map.device.tick();
        }

        let performed = self.last_op;
        self.last_op = BusOp::Internal; // Reset operation for next cycle
        performed
    }

    pub fn rdy(&self) -> bool {
        self.rdy
    }
}
