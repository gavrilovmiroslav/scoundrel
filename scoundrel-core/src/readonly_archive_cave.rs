use std::fs::{remove_file, File};
use std::path::Path;

use caves::res::Res;
use caves::Cave;
use filearco::v1::FileArco;

pub struct ReadonlyArchiveCave {
    archive: FileArco,
}

impl Cave for ReadonlyArchiveCave {
    fn get(&self, name: &str) -> Res {
        let file = self.archive.get(name).unwrap();
        let data = file.as_slice().to_vec();
        Ok(data)
    }

    fn set(&self, _name: &str, _data: &[u8]) -> Res {
        panic!("You're attempting to set values in a read-only archive.");
    }

    fn delete(&self, _name: &str) -> Res {
        panic!("You're attempting to delete values from a read-only archive.");
    }
}

impl ReadonlyArchiveCave {
    pub fn make_from(dir: &str, output: &str) {
        let data_path = Path::new(dir);
        let output_path = Path::new(output);
        remove_file(output_path).ok();

        let output = File::create(output_path).unwrap();
        let file_data = filearco::get_file_data(data_path).unwrap();
        FileArco::make(file_data, output).unwrap();
    }

    pub fn open(path: &'static str) -> ReadonlyArchiveCave {
        let archive = FileArco::new(path).unwrap();
        ReadonlyArchiveCave { archive }
    }
}
