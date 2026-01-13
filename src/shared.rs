use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

pub type Word = u16;
pub type Byte = u8;

pub fn read_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    let path = path.as_ref();
    let mut file = File::open(path).map_err(|e| {
        io::Error::new(
            e.kind(),
            format!("Failed to open file '{}': {}", path.display(), e),
        )
    })?;

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).map_err(|e| {
        io::Error::new(
            e.kind(),
            format!("Failed to read file '{}': {}", path.display(), e),
        )
    })?;

    Ok(buffer)
}
