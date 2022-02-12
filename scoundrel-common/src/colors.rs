
#[derive(Copy, Clone)]
#[no_mangle]
#[repr(C)]
pub struct Color {
    pub hue: u8,
    pub sat: u8,
    pub val: u8,
    pub alf: u8,
}