use std::io::Write;
use std::path::PathBuf;
use vfs::impls::overlay::OverlayFS;
use vfs::{FileSystem, MemoryFS, PhysicalFS, VfsPath};

struct LibraryFileSystem {
    fs: OverlayFS,
}

pub struct FileSystemBuilder {
    layers: Vec<VfsPath>,
    top_layer: VfsPath,
}

impl FileSystemBuilder {
    pub fn new() -> Self {
        FileSystemBuilder {
            layers: vec![],
            top_layer: MemoryFS::new().into(),
        }
    }

    pub fn add_physical_layer(mut self, path: impl Into<PathBuf>) -> Self {
        self.layers.push(PhysicalFS::new(path.into()).into());
        self
    }

    pub fn add_virtual_file(self, path: &str, content: &[u8]) -> Self {
        let path = path.trim_start_matches("/");
        let full_path = self.top_layer.join(path).unwrap();
        full_path.parent().unwrap().create_dir_all().unwrap();
        full_path.create_file().unwrap().write_all(content).unwrap();
        self
    }
}

impl From<FileSystemBuilder> for LibraryFileSystem {
    fn from(fsb: FileSystemBuilder) -> Self {
        LibraryFileSystem { fs: fsb.into() }
    }
}

impl From<FileSystemBuilder> for OverlayFS {
    fn from(fsb: FileSystemBuilder) -> Self {
        let mut layers = fsb.layers;
        layers.push(fsb.top_layer);
        layers.reverse();
        OverlayFS::new(&layers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn overlay_fs() {
        let fs = OverlayFS::new(&[MemoryFS::new().into()]);

        let root: VfsPath = fs.into();
        let path = root.join("foo/bar.sld").unwrap();

        path.parent().unwrap().create_dir_all().unwrap();

        println!("{:?}", path);

        {
            let mut w = path.create_file().unwrap();
            w.write_all(b"hello, world!").unwrap();
        }

        assert!(path.exists().unwrap());
    }

    #[test]
    fn build_empty_filesystem() {
        let fs: OverlayFS = FileSystemBuilder::new().into();
        assert!(fs.exists("").unwrap());
    }

    #[test]
    fn build_physical_filesystem() {
        let fs: OverlayFS = FileSystemBuilder::new().add_physical_layer("src").into();
        for d in fs.read_dir("").unwrap() {
            println!("{}", d);
        }
        assert!(fs.exists("/library_filesystem.rs").unwrap());
    }

    #[test]
    fn build_physical_filesystem_with_virtual_file() {
        let fs: OverlayFS = FileSystemBuilder::new()
            .add_physical_layer("src")
            .add_virtual_file("/foo/bar.txt", b"Hello, World!")
            .into();
        for d in fs.read_dir("").unwrap() {
            println!("{}", d);
        }
        assert!(fs.exists("/foo/bar.txt").unwrap());
    }
}
