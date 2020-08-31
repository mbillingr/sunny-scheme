use std::fs::File;
use std::io::{Read, BufReader, Stdin, Write};
use std::path::Path;
use std::collections::VecDeque;

pub trait InputPort: ScmReader {
    fn is_open(&self) -> bool;
    fn close(&mut self);
}

pub trait OutputPort: ScmWriter {
    fn is_open(&self) -> bool;
    fn close(&mut self);
}

impl InputPort for Stdin {
    fn is_open(&self) -> bool { true }
    fn close(&mut self) {}
}

pub struct MemoryInputPort {
    bytes: VecDeque<u8>
}

impl MemoryInputPort {
    pub fn from_str(s: &str) -> Self {
        MemoryInputPort {
            bytes: s.to_owned().into_bytes().into()
        }
    }
}

impl InputPort for MemoryInputPort {
    fn is_open(&self) -> bool { true }
    fn close(&mut self) {}
}

impl Read for MemoryInputPort {
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        let mut n = 0;
        while buf.len() > 0 {
            if let Some(b) = self.bytes.pop_front() {
                buf[0] = b;
                buf = &mut buf[1..];
                n += 1;
            } else {
                break
            }
        }
        Ok(n)
    }
}

pub struct FileInputPort {
    handle: Option<BufReader<File>>,
}

impl FileInputPort {
    pub fn open<P: AsRef<Path>>(path: P) -> Self {
        FileInputPort {
            handle: Some(BufReader::new(File::open(path).unwrap())),
        }
    }
}

impl InputPort for FileInputPort {
    fn is_open(&self) -> bool { self.handle.is_some() }
    fn close(&mut self) {
        self.handle = None;
    }
}

impl Read for FileInputPort {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match &mut self.handle {
            Some(f) => f.read(buf),
            None => Ok(0),
        }
    }
}

pub trait ScmReader: Read {
    fn read_char(&mut self) -> Option<char>;
    fn read_line(&mut self) -> Option<String>;
    fn read_string(&mut self, k: usize) -> Option<String>;
    fn read_u8(&mut self) -> Option<u8>;
    fn read_bytevector(&mut self, k: usize) -> Option<Vec<u8>>;
}

impl<T: Read> ScmReader for T {
    fn read_char(&mut self) -> Option<char> {
        let mut bytes = [0; 4];
        let n = self.read(&mut bytes[0..1]).unwrap();

        if n == 0 {
            return None;
        }

        let codepoint = match bytes[0] {
            b if b < 0b10000000 => &bytes[0..1],
            b if b < 0b11100000 => {
                self.read_exact(&mut bytes[1..2]).unwrap();
                &bytes[0..2]
            }
            b if b < 0b11110000 => {
                self.read_exact(&mut bytes[1..3]).unwrap();
                &bytes[0..3]
            }
            b if b < 0b11111000 => {
                self.read_exact(&mut bytes[1..4]).unwrap();
                &bytes[0..4]
            }
            _ => panic!("Invalid UTF-8 data"),
        };

        Some(
            std::str::from_utf8(codepoint)
                .unwrap()
                .chars()
                .next()
                .unwrap(),
        )
    }

    fn read_line(&mut self) -> Option<String> {
        let mut buffer = vec![];

        while buffer.last() != Some(&b'\n') {
            let n = buffer.len();
            buffer.push(0);
            if self.read(&mut buffer[n..]).unwrap() == 0 {
                if buffer.len() == 1 {
                    // Eof
                    return None;
                }
                break
            }
        }

        buffer.pop(); // remove trailing newline character
        Some(String::from_utf8(buffer).unwrap())
    }

    fn read_string(&mut self, mut k: usize) -> Option<String> {
        let mut buffer = vec![];

        while k > 0 {
            let n = buffer.len();
            buffer.push(0);
            if self.read(&mut buffer[n..]).unwrap() == 0 {
                if buffer.len() == 1 {
                    // Eof
                    return None
                }
                break
            }

            let n = buffer.len();
            match *buffer.last().unwrap() {
                b if b < 0b10000000 => {}
                b if b < 0b11100000 => {
                    buffer.resize(n + 1, 0);
                    self.read_exact(&mut buffer[n..]).unwrap()
                },
                b if b < 0b11110000 => {
                    buffer.resize(n + 2, 0);
                    self.read_exact(&mut buffer[n..]).unwrap()
                },
                b if b < 0b11111000 => {
                    buffer.resize(n + 3, 0);
                    self.read_exact(&mut buffer[n..]).unwrap()
                },
                _ => panic!("Invalid UTF-8 data"),
            };

            k -= 1;
        }

        Some(String::from_utf8(buffer).unwrap())
    }

    fn read_u8(&mut self) -> Option<u8> {
        let mut buffer = [0];
        if self.read(&mut buffer).unwrap() == 0 {
            None
        } else {
            Some(buffer[0])
        }
    }

    fn read_bytevector(&mut self, k: usize) -> Option<Vec<u8>> {
        let mut buffer = vec![0; k];
        let n = self.read(&mut buffer).unwrap();
        if n == 0 {
            None
        } else {
            buffer.truncate(n);
            Some(buffer)
        }
    }
}

pub trait ScmWriter: Write {
    fn write_char(&mut self, ch: char);
    fn write_str(&mut self, s: &str);
    fn write_u8(&mut self, ch: u8);
    fn write_bytevector(&mut self, buffer: &[u8]);
}

impl<T: Write> ScmWriter for T {
    fn write_char(&mut self, ch: char) {
        let mut buffer = [0; 4];
        let s = ch.encode_utf8(&mut buffer);
        self.write_str(s)
    }

    fn write_str(&mut self, s: &str) {
        self.write_bytevector(s.as_bytes())
    }

    fn write_u8(&mut self, u: u8) {
        let buffer = [u];
        self.write_bytevector(&buffer)
    }

    fn write_bytevector(&mut self, buffer: &[u8]) {
        self.write_all(buffer).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_ascii_chars() {
        let mut s = "foo bar".as_bytes();
        assert_eq!(s.read_char(), Some('f'));
        assert_eq!(s.read_char(), Some('o'));
        assert_eq!(s.read_char(), Some('o'));
        assert_eq!(s.read_char(), Some(' '));
        assert_eq!(s.read_char(), Some('b'));
        assert_eq!(s.read_char(), Some('a'));
        assert_eq!(s.read_char(), Some('r'));
        assert_eq!(s.read_char(), None);
    }

    #[test]
    fn read_utf8_2_chars() {
        let mut s = "äöü".as_bytes();
        assert_eq!(s.read_char(), Some('ä'));
        assert_eq!(s.read_char(), Some('ö'));
        assert_eq!(s.read_char(), Some('ü'));
        assert_eq!(s.read_char(), None);
    }

    #[test]
    fn read_utf8_3_chars() {
        let mut s = "€".as_bytes();
        assert_eq!(s.read_char(), Some('€'));
        assert_eq!(s.read_char(), None);
    }

    #[test]
    fn read_utf8_4_chars() {
        let mut s = "\u{1D11E}".as_bytes();
        assert_eq!(s.read_char(), Some('𝄞'));
        assert_eq!(s.read_char(), None);
    }

    #[test]
    fn read_line() {
        let mut s = "foo®\n€€€\n\n".as_bytes();
        assert_eq!(s.read_line(), Some("foo®".to_owned()));
        assert_eq!(s.read_line(), Some("€€€".to_owned()));
        assert_eq!(s.read_line(), Some("".to_owned()));
        assert_eq!(s.read_line(), None);
    }

    #[test]
    fn read_string() {
        let mut s = "foo®\n€€€\n\n".as_bytes();
        assert_eq!(s.read_string(5), Some("foo®\n".to_owned()));
        assert_eq!(s.read_string(3), Some("€€€".to_owned()));
        assert_eq!(s.read_string(2), Some("\n\n".to_owned()));
        assert_eq!(s.read_string(1), None);
    }

    #[test]
    fn read_bytes() {
        let mut s = "foo\n".as_bytes();
        assert_eq!(s.read_u8(), Some(b'f'));
        assert_eq!(s.read_u8(), Some(b'o'));
        assert_eq!(s.read_u8(), Some(b'o'));
        assert_eq!(s.read_u8(), Some(b'\n'));
        assert_eq!(s.read_u8(), None);
    }

    #[test]
    fn read_bytevec() {
        let mut s = "foo".as_bytes();
        assert_eq!(s.read_bytevector(2), Some(b"fo".to_vec()));
        assert_eq!(s.read_bytevector(2), Some(b"o".to_vec()));
        assert_eq!(s.read_bytevector(2), None);
    }
}
