use crate::shared::{Byte, Word};

pub trait Device {
    fn maps(&self, addr: Word) -> bool;
    fn read(&mut self, addr: Word) -> Byte;
    fn write(&mut self, addr: Word, val: Byte);
    fn tick(&mut self);
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
    fn maps(&self, _addr: Word) -> bool {
        true
    }
    fn read(&mut self, addr: Word) -> Byte {
        self.data[addr as usize]
    }

    fn write(&mut self, addr: Word, val: Byte) {
        self.data[addr as usize] = val;
    }

    fn tick(&mut self) {}
}

#[derive(Clone, Copy)]
pub enum BusOp {
    Read(Word),
    Write(Word, Byte),
    Internal,
}

pub struct Bus {
    devices: Vec<Box<dyn Device>>,
    last_op: BusOp,
    data_bus: Byte,
    rdy: bool,
}

impl Bus {
    pub fn new() -> Self {
        Self {
            devices: Vec::new(),
            last_op: BusOp::Internal,
            data_bus: 0xFF,
            rdy: true,
        }
    }

    pub fn attach_device<D: Device + 'static>(&mut self, device: D) {
        self.devices.push(Box::new(device));
    }

    fn find_device_mut(&mut self, addr: Word) -> Option<&mut dyn Device> {
        for dev in self.devices.iter_mut() {
            if dev.maps(addr) {
                return Some(dev.as_mut());
            }
        }

        None
    }

    pub fn read(&mut self, addr: Word) -> Byte {
        self.last_op = BusOp::Read(addr);

        if let Some(dev) = self.find_device_mut(addr) {
            let val = dev.read(addr);
            self.data_bus = val;
            val
        } else {
            self.data_bus
        }
    }

    pub fn write(&mut self, addr: Word, val: Byte) {
        self.last_op = BusOp::Write(addr, val);
        self.data_bus = val;

        if let Some(dev) = self.find_device_mut(addr) {
            dev.write(addr, val);
        }
    }

    pub fn tick(&mut self) -> BusOp {
        for dev in self.devices.iter_mut() {
            dev.tick();
        }

        let performed = self.last_op;
        self.last_op = BusOp::Internal; // Reset operation for next cycle
        performed
    }

    pub fn rdy(&self) -> bool {
        self.rdy
    }
}
