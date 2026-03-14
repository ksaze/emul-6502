mod core;
mod execute;
mod flags;
mod signals;
mod stack;
mod state;

use crate::core::variants::{Decoder, Quirks};

pub(super) use core::CPUCore;
pub(super) use flags::Status;
pub(super) use signals::Signals;
pub(super) use stack::StackPointer;
pub(super) use state::CPUState;

pub struct CPU<V: Decoder + Quirks> {
    pub core: CPUCore,
    pub decoder: V,
}

#[cfg(feature = "test-utils")]
#[allow(unused_imports)]
pub mod test_utils {
    pub use super::flags::Status;
    pub use super::state::CPUState;
}
