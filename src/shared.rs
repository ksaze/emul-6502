use std::cell::RefCell;
use std::rc::Rc;

pub type Word = u16;
pub type Byte = u8;
pub type SharedDevice<T> = Rc<RefCell<T>>;
