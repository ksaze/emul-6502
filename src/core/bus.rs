use std::{cell::RefCell, rc::Rc};

use crate::shared::{Byte, SharedDevice, Word};

pub trait Device {
    fn read(&mut self, addr: Word) -> Byte;
    fn write(&mut self, addr: Word, val: Byte);
    fn tick(&mut self);

    fn nmi(&self) -> bool {
        true
    }
    fn irq(&self) -> bool {
        true
    }
    fn res(&self) -> bool {
        true
    }
    fn rdy(&self) -> bool {
        true
    }
    fn into_shared(self) -> SharedDevice<Self>
    where
        Self: Sized + 'static,
    {
        Rc::new(RefCell::new(self)) as SharedDevice<Self>
    }
}

struct BusMapping {
    pub base: Word,
    pub mask: Word,
    pub device: SharedDevice<dyn Device>,
}

impl BusMapping {
    fn maps(&self, addr: Word) -> bool {
        (addr & self.mask) == self.base
    }

    fn offset(&self, addr: Word) -> Word {
        addr & !self.mask
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BusOp {
    Read(Word, Byte),
    Write(Word, Byte),
    Internal,
}

pub struct Bus {
    mappings: Vec<BusMapping>,
    data_bus: Byte,
    addr_bus: Word,
    pub last_op: BusOp,
    pub irq: bool,
    pub nmi: bool,
    pub res: bool,
    pub rdy: bool,
}

impl Bus {
    pub fn new() -> Self {
        Self {
            mappings: Vec::new(),
            last_op: BusOp::Internal,
            data_bus: 0xFF,
            addr_bus: 0xFF,
            irq: true,
            nmi: true,
            res: true,
            rdy: true,
        }
    }

    // Device is not cloned
    pub fn attach_device<D: Device + 'static>(&mut self, device: D, base: Word, mask: Word) {
        self.mappings.push(BusMapping {
            base,
            mask,
            device: device.into_shared(),
        });
    }

    pub fn attach_shared_device<D: Device + 'static>(
        &mut self,
        device: &SharedDevice<D>,
        base: Word,
        mask: Word,
    ) {
        self.mappings.push(BusMapping {
            base,
            mask,
            device: device.clone(),
        });
    }

    fn find_device_mut(&mut self, addr: Word) -> Option<&mut BusMapping> {
        self.mappings.iter_mut().find(|map| map.maps(addr))
    }

    pub(super) fn read(&mut self, addr: Word) -> Byte {
        if self.last_op != BusOp::Internal {
            panic!("multiple bus operations in one cycle.");
        }

        if let Some(map) = self.find_device_mut(addr) {
            let val = map.device.borrow_mut().read(map.offset(addr));
            self.data_bus = val;
            self.last_op = BusOp::Read(addr, val);
            self.addr_bus = addr;
            val
        } else {
            self.last_op = BusOp::Read(addr, self.data_bus);
            self.data_bus
        }
    }

    pub(super) fn write(&mut self, addr: Word, val: Byte) {
        if self.last_op != BusOp::Internal {
            panic!("multiple bus operations in one cycle.");
        }

        self.last_op = BusOp::Write(addr, val);
        self.data_bus = val;
        self.addr_bus = addr;

        if let Some(map) = self.find_device_mut(addr) {
            map.device.borrow_mut().write(map.offset(addr), val);
        }
    }

    #[cfg(feature = "test-utils")]
    pub fn read_raw(&mut self, addr: Word) -> Byte {
        if let Some(map) = self.find_device_mut(addr) {
            let val = map.device.borrow_mut().read(map.offset(addr));
            val
        } else {
            self.data_bus
        }
    }

    #[cfg(feature = "test-utils")]
    pub fn write_raw(&mut self, addr: Word, val: Byte) {
        if let Some(map) = self.find_device_mut(addr) {
            map.device.borrow_mut().write(map.offset(addr), val);
        }
    }

    pub fn tick(&mut self) {
        self.irq = true;
        self.nmi = true;
        self.res = true;

        for map in self.mappings.iter_mut() {
            let mut device = map.device.borrow_mut();

            device.tick();
            self.irq &= device.irq();
            self.nmi &= device.nmi();
            self.res &= device.res();
            self.rdy &= device.rdy();
        }

        self.last_op = BusOp::Internal; // reset operation for next cycle
    }
}
