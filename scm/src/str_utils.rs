pub fn line_number(src: &str, offset: usize) -> usize {
    src.bytes()
        .enumerate()
        .filter(|(_, ch)| *ch == b'\n')
        .map(|(i, _)| i)
        .take_while(|&i| i < offset)
        .count()
}

pub fn find_end_of_line(src: &str, offset: usize) -> usize {
    src[offset..]
        .find('\n')
        .map(|len| offset + len)
        .unwrap_or_else(|| src.len())
}

pub fn find_start_of_line(src: &str, offset: usize) -> usize {
    src.bytes()
        .enumerate()
        .filter(|(_, ch)| *ch == b'\n')
        .map(|(i, _)| i)
        .take_while(|&i| i < offset)
        .last()
        .map(|i| i + 1)
        .unwrap_or(0)
}
