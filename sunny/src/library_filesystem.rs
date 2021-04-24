use std::io::Write;
use vfs::impls::overlay::OverlayFS;
use vfs::{MemoryFS, PhysicalFS, VfsPath};

#[derive(Debug, Clone)]
pub struct LibraryFileSystem {
    root: VfsPath,
}

impl Default for LibraryFileSystem {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl LibraryFileSystem {
    pub fn new(lib_paths: Vec<&str>) -> Self {
        let pyhsical_fs = VfsPath::new(PhysicalFS::new("/".into()));
        let mut layers = vec![MemoryFS::new().into()];
        layers.extend(
            lib_paths
                .into_iter()
                .map(|path| path.trim_start_matches('/'))
                .map(|path| pyhsical_fs.join(path).unwrap())
                .map(Into::into),
        );
        LibraryFileSystem {
            root: OverlayFS::new(&layers).into(),
        }
    }

    pub fn add_virtual_file(&self, path: &str, content: &[u8]) {
        let path = path.trim_start_matches('/');
        let full_path = self.root.join(path).unwrap();
        assert!(!full_path.exists().unwrap());
        full_path.parent().unwrap().create_dir_all().unwrap();
        full_path.create_file().unwrap().write_all(content).unwrap();
    }

    pub fn map_libname_to_path(&self, libname: &str) -> String {
        let parts: Vec<&str> = libname
            .trim_start_matches('(')
            .trim_end_matches(')')
            .split_whitespace()
            .collect();

        parts.join("/") + ".sld"
    }

    pub fn load_string(&self, path: &str) -> Option<String> {
        let mut buf = String::new();

        self.root
            .join(path)
            .ok()
            .and_then(|path| path.open_file().ok())
            .and_then(|mut file| file.read_to_string(&mut buf).ok())
            .map(|_| buf)
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
    fn lfs_with_virtual_file() {
        let lfs = LibraryFileSystem::new(vec![]);
        lfs.add_virtual_file("lib/a.txt", b"A");
        assert_eq!(lfs.load_string("lib/a.txt"), Some("A".to_string()));
    }

    #[test]
    fn lfs_with_multiple_virtual_files() {
        let lfs = LibraryFileSystem::new(vec![]);
        lfs.add_virtual_file("lib/a.txt", b"A");
        lfs.add_virtual_file("lib/b.txt", b"B");
        assert_eq!(lfs.load_string("lib/b.txt"), Some("B".to_string()));
        assert_eq!(lfs.load_string("lib/a.txt"), Some("A".to_string()));
    }
}
