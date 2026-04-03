use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use crate::core::bus::{BusMaster, Device, DeviceHandle};
use crate::devices::DMAController;

pub trait IntoHandle<H> {
    type ConcreteHandle: DeviceHandle<Self>
    where
        Self: 'static;

    fn into_handle(self) -> (H, Option<Self::ConcreteHandle>);
}

pub trait IntoDMAHandle<H>: IntoHandle<H> {
    fn into_dma_handles(self) -> (H, Option<Box<dyn DMAController>>);
}

/// Bus-owned device
pub type OwnedDevice<D> = Box<D>;
pub type OwnedDeviceDyn = OwnedDevice<dyn Device>;

/// Box<D> implementation
impl<D: Device + ?Sized + 'static> DeviceHandle<D> for OwnedDevice<D> {
    fn with<R>(&mut self, f: impl for<'a> FnOnce(&'a mut D) -> R) -> R {
        f(self)
    }
}

impl<D: Device + 'static> IntoHandle<OwnedDeviceDyn> for D {
    type ConcreteHandle = OwnedDevice<D>;

    fn into_handle(self) -> (OwnedDeviceDyn, Option<Self::ConcreteHandle>) {
        let dev_handle = Box::new(self);
        (dev_handle, None)
    }
}

impl<D: Device + DMAController + 'static> IntoDMAHandle<OwnedDevice<dyn Device>> for D {
    fn into_dma_handles(self) -> (OwnedDevice<dyn Device>, Option<Box<dyn DMAController>>) {
        let handle = Box::new(self);
        (handle, None)
    }
}

/// Single-threaded shared device
pub type SharedDevice<D> = Rc<RefCell<D>>;
pub type SharedDeviceDyn = SharedDevice<dyn Device>;

impl<D: Device + ?Sized + 'static> DeviceHandle<D> for SharedDevice<D> {
    fn with<R>(&mut self, f: impl for<'a> FnOnce(&'a mut D) -> R) -> R {
        f(&mut *self.borrow_mut())
    }
}

impl<D: Device + 'static> IntoHandle<SharedDeviceDyn> for D {
    type ConcreteHandle = SharedDevice<D>;

    fn into_handle(self) -> (SharedDeviceDyn, Option<Self::ConcreteHandle>) {
        let concrete_handle = Rc::new(RefCell::new(self));
        let dev_handle = concrete_handle.clone() as SharedDeviceDyn;
        (dev_handle, Some(concrete_handle))
    }
}

impl<D: Device + DMAController + 'static> IntoDMAHandle<SharedDevice<dyn Device>> for D {
    fn into_dma_handles(self) -> (SharedDevice<dyn Device>, Option<Box<dyn DMAController>>) {
        let handle = Rc::new(RefCell::new(self));
        let dma: Box<dyn DMAController> = Box::new(handle.clone());
        (handle, Some(dma))
    }
}

impl<D: DMAController + ?Sized + 'static> DMAController for SharedDevice<D> {
    fn wants_bus(&mut self) -> bool {
        self.borrow_mut().wants_bus()
    }
    fn dma_tick(&mut self, bus: &mut dyn BusMaster) {
        self.borrow_mut().dma_tick(bus)
    }
}

/// Multi-threaded shared device
pub type SyncDevice<D> = Arc<Mutex<D>>;
pub type SyncDeviceDyn = Arc<Mutex<dyn Device>>;

impl<D: Device + ?Sized + 'static> DeviceHandle<D> for SyncDevice<D> {
    fn with<R>(&mut self, f: impl for<'a> FnOnce(&'a mut D) -> R) -> R {
        let mut guard = self.lock().unwrap();
        f(&mut *guard)
    }
}

impl<D: Device + 'static> IntoHandle<SyncDeviceDyn> for D {
    type ConcreteHandle = SyncDevice<D>;

    fn into_handle(self) -> (SyncDeviceDyn, Option<Self::ConcreteHandle>) {
        let concrete_handle = Arc::new(Mutex::new(self));
        let dev_handle = concrete_handle.clone() as SyncDeviceDyn;
        (dev_handle, Some(concrete_handle))
    }
}

impl<D: Device + DMAController + 'static> IntoDMAHandle<SyncDevice<dyn Device>> for D {
    fn into_dma_handles(self) -> (SyncDevice<dyn Device>, Option<Box<dyn DMAController>>) {
        let handle = Arc::new(Mutex::new(self));
        let dma: Box<dyn DMAController> = Box::new(handle.clone());
        (handle, Some(dma))
    }
}

impl<D: DMAController + ?Sized + 'static> DMAController for SyncDevice<D> {
    fn wants_bus(&mut self) -> bool {
        self.lock().unwrap().wants_bus()
    }
    fn dma_tick(&mut self, bus: &mut dyn BusMaster) {
        self.lock().unwrap().dma_tick(bus)
    }
}
