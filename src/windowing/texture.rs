use std::error::Error;
use std::ffi::c_void;
use std::fmt::{Debug, Display, Formatter};

use image::GenericImageView;

use crate::core::presentation::Presentation;

pub struct Texture {
    pub width: u32,
    pub height: u32,
    pub texture_id: u32,
    pub glyph_size: (u32, u32),
}

#[derive(Debug)]
pub enum TextureError {
    LoadTextureFailed,
}

impl Display for TextureError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Error for TextureError {}

impl Texture {
    pub fn load(render_options: &Presentation) -> Result<Texture, TextureError> {
        let image = image::open(format!(
            "resources/data/{}",
            render_options.input_font.as_str()
        ))
        .map_err(|_| TextureError::LoadTextureFailed)?;
        let dimensions = image.dimensions();
        let reversed_data: Vec<u8> = image
            .to_rgba8()
            .into_raw()
            .chunks(dimensions.0 as usize * 4)
            //            .rev() <------ reversal of image was needed in Crow, why not here?
            .flat_map(|row| row.iter())
            .copied()
            .collect();

        let mut id = 0;
        unsafe {
            gl::GenTextures(1, &mut id);

            gl::BindTexture(gl::TEXTURE_2D, id);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::NEAREST as _);
            gl::TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::NEAREST as _);
            gl::TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGBA8 as _,
                dimensions.0 as _,
                dimensions.1 as _,
                0,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                reversed_data.as_ptr() as *const c_void,
            );
        }

        Ok(Texture {
            texture_id: id,
            width: dimensions.0,
            height: dimensions.1,
            glyph_size: render_options.input_font_glyph_size,
        })
    }

    pub fn bind(&self) {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, self.texture_id);
        }
    }

    pub fn unbind(&self) {
        unsafe {
            gl::BindTexture(gl::TEXTURE_2D, 0);
        }
    }
}

impl Drop for Texture {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteTextures(1, &self.texture_id);
        }
    }
}
