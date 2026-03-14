#[inline]
pub(super) fn aaa(op: u8) -> u8 {
    (op & 0xE0) >> 5
}

#[inline]
pub(super) fn bbb(op: u8) -> u8 {
    (op & 0x1C) >> 2
}

#[inline]
pub(super) fn cc(op: u8) -> u8 {
    op & 0x03
}

#[inline]
pub(super) fn lnibble(op: u8) -> u8 {
    op & 0x0F
}

#[inline]
pub(super) fn hnibble(op: u8) -> u8 {
    (op & 0xF0) >> 4
}
