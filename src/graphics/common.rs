
pub fn gl_error_check() {
    let gl_error = unsafe { gl::GetError() };
    match gl_error {
        gl::NO_ERROR => (),
        gl::OUT_OF_MEMORY => {
            // OpenGl is now in an undefined state,
            // consider aborting instead, as it is possible
            // to catch a panic
            panic!("OpenGl is out of memory and in an invalid state");
        }
        gl::INVALID_VALUE => {
            panic!("OpenGl has found an invalid value!");
        }
        gl::INVALID_OPERATION => {
            panic!("OpenGl has encountered an invalid operation!");
        }
        e => panic!("unexpected error: {}", e),
    }
}
