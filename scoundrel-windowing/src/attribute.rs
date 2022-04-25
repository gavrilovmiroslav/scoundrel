use gl::types::*;
use std::ffi::c_void;

#[derive(Clone, Copy)]
pub struct AttribPosition(pub GLuint);

impl Into<u32> for AttribPosition {
    fn into(self) -> u32 {
        self.0
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy)]
pub enum AttribSize {
    One,
    Two,
    Three,
    Four,
}

impl AttribSize {
    pub fn as_i32(self) -> i32 {
        match self {
            AttribSize::One => 1,
            AttribSize::Two => 2,
            AttribSize::Three => 3,
            AttribSize::Four => 4,
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy)]
pub enum AttribType {
    Byte,
    UnsignedByte,
    HalfFloat,
    Short,
    UnsignedShort,
    Float,
    Int,
    UnsignedInt,
    Int2101010Rev,
    UnsignedInt2101010Rev,
    UnsignedInt10F11F11FRev,
    Double,
}

impl AttribType {
    pub fn is_int(&self) -> bool {
        match self {
            AttribType::Byte
            | AttribType::UnsignedByte
            | AttribType::Short
            | AttribType::UnsignedShort
            | AttribType::Int
            | AttribType::UnsignedInt => true,

            _ => false,
        }
    }

    pub fn stride(self) -> i32 {
        use crate::attribute::AttribType::*;

        match self {
            Byte | UnsignedByte => 1,
            HalfFloat | Short | UnsignedShort => 2,
            Float | Int | UnsignedInt | Int2101010Rev => 4,
            UnsignedInt2101010Rev | UnsignedInt10F11F11FRev => 4,
            Double => 8,
        }
    }
}

impl Into<GLenum> for AttribType {
    fn into(self) -> GLenum {
        use crate::attribute::AttribType::*;

        match self {
            Byte => gl::BYTE,
            UnsignedByte => gl::UNSIGNED_BYTE,
            HalfFloat => gl::HALF_FLOAT,
            Short => gl::SHORT,
            UnsignedShort => gl::UNSIGNED_SHORT,
            Float => gl::FLOAT,
            Int => gl::INT,
            UnsignedInt => gl::UNSIGNED_INT,
            Int2101010Rev => gl::INT_2_10_10_10_REV,
            UnsignedInt2101010Rev => gl::UNSIGNED_INT_2_10_10_10_REV,
            UnsignedInt10F11F11FRev => gl::UNSIGNED_INT_10F_11F_11F_REV,
            Double => gl::DOUBLE,
        }
    }
}

pub struct Attribute {
    pub position: AttribPosition,
    pub size: AttribSize,
    pub kind: AttribType,
}

pub struct BufferMapping {
    pub attributes: Vec<Attribute>,
    pub divisor: bool,
}

impl BufferMapping {
    pub fn new_static() -> Self {
        BufferMapping {
            attributes: Vec::new(),
            divisor: false,
        }
    }

    pub fn new_instanced() -> Self {
        BufferMapping {
            attributes: Vec::new(),
            divisor: true,
        }
    }

    pub fn with_attrib(
        &mut self,
        _name: &str,
        position: AttribPosition,
        size: AttribSize,
        kind: AttribType,
    ) -> &mut Self {
        let attrib = Attribute {
            position,
            size,
            kind,
        };
        self.attributes.push(attrib);
        self
    }

    pub fn build_bound_buffer(&self, starting_pointer: i32) {
        let mut pointer = starting_pointer;

        unsafe {
            let stride = self
                .attributes
                .iter()
                .fold(0, |all, x| all + x.size.as_i32() * x.kind.stride());

            for attrib in &self.attributes {
                let glen: GLenum = attrib.kind.into();

                gl::EnableVertexAttribArray(attrib.position.into());

                if attrib.kind.is_int() {
                    gl::VertexAttribIPointer(
                        attrib.position.0,
                        attrib.size.as_i32(),
                        glen,
                        stride,
                        pointer as *const c_void,
                    );
                } else {
                    gl::VertexAttribPointer(
                        attrib.position.0,
                        attrib.size.as_i32(),
                        glen,
                        gl::FALSE as GLboolean,
                        stride,
                        pointer as *const c_void,
                    );
                }

                if self.divisor {
                    gl::VertexAttribDivisor(attrib.position.0, 1);
                }

                pointer += attrib.size.as_i32() * attrib.kind.stride();
            }
        }
    }
}
