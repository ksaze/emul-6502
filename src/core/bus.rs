use crate::core::{Byte, Word};

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
}

pub trait DeviceHandle<D: ?Sized + 'static> {
    fn with<R>(&mut self, f: impl for<'a> FnOnce(&'a mut D) -> R) -> R;
}

pub struct BusMapping<H: DeviceHandle<dyn Device>> {
    pub map_base: Word,
    pub map_mask: Word,
    pub addr_mask: Word,
    pub device: H,
}

impl<H: DeviceHandle<dyn Device>> BusMapping<H> {
    fn maps(&self, addr: Word) -> bool {
        (addr & self.map_mask) == self.map_base
    }

    fn offset(&self, addr: Word) -> Word {
        addr & !self.addr_mask
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BusOp {
    Read(Word, Byte),
    Write(Word, Byte),
    Internal,
}

pub struct Bus<H: DeviceHandle<dyn Device>> {
    mappings: Vec<BusMapping<H>>,
    data_bus: Byte,
    addr_bus: Word,
    pub last_op: BusOp,
    pub(super) irq: bool,
    pub(super) nmi: bool,
    pub(super) res: bool,
    pub rdy: bool,
}

impl<H: DeviceHandle<dyn Device>> Bus<H> {
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

    pub fn attach_device_handle(
        &mut self,
        device: H,
        map_base: Word,
        map_mask: Word,
        addr_mask: Word,
    ) {
        self.mappings.push(BusMapping {
            map_base,
            map_mask,
            addr_mask,
            device,
        })
    }

    fn find_device_mut(&mut self, addr: Word) -> Option<&mut BusMapping<H>> {
        self.mappings.iter_mut().find(|map| map.maps(addr))
    }

    pub(super) fn read(&mut self, addr: Word) -> Byte {
        if self.last_op != BusOp::Internal {
            panic!("multiple bus operations in one cycle.");
        }

        if let Some(map) = self.find_device_mut(addr) {
            let offset = map.offset(addr);
            let val = map.device.with(|dev| dev.read(offset));
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
            let offset = map.offset(addr);
            map.device.with(|dev| dev.write(offset, val));
        }
    }

    #[cfg(feature = "test-utils")]
    pub fn read_raw(&mut self, addr: Word) -> Byte {
        if let Some(map) = self.find_device_mut(addr) {
            let offset = map.offset(addr);
            let val = map.device.with(|dev| dev.read(offset));
            val
        } else {
            self.data_bus
        }
    }

    #[cfg(feature = "test-utils")]
    pub fn write_raw(&mut self, addr: Word, val: Byte) {
        if let Some(map) = self.find_device_mut(addr) {
            let offset = map.offset(addr);
            map.device.with(|dev| dev.write(offset, val));
        }
    }

    pub fn tick(&mut self) {
        self.irq = true;
        self.nmi = true;
        self.res = true;
        self.rdy = true;

        for map in self.mappings.iter_mut() {
            map.device.with(|device| {
                device.tick();
                self.irq &= device.irq();
                self.nmi &= device.nmi();
                self.res &= device.res();
                self.rdy &= device.rdy();
            })
        }

        self.last_op = BusOp::Internal; // reset operation for next cycle
    }
}

// Interface exposed to Bus Masters (CPU, DMA)
pub trait BusMaster {
    fn read(&mut self, addr: Word) -> Byte;
    fn write(&mut self, addr: Word, val: Byte);

    fn rdy(&self) -> bool;
    fn irq(&self) -> bool;
    fn nmi(&self) -> bool;
    fn res(&self) -> bool;
}

impl<H: DeviceHandle<dyn Device>> BusMaster for Bus<H> {
    fn read(&mut self, addr: Word) -> Byte {
        self.read(addr)
    }
    fn write(&mut self, addr: Word, val: Byte) {
        self.write(addr, val)
    }

    fn rdy(&self) -> bool {
        self.rdy
    }
    fn irq(&self) -> bool {
        self.irq
    }
    fn nmi(&self) -> bool {
        self.nmi
    }
    fn res(&self) -> bool {
        self.res
    }
}
