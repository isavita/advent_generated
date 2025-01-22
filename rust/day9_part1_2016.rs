
use std::fs::File;
use std::io::{self, Read};

fn decompress_len(input: &str) -> usize {
    let mut len = 0;
    let mut chars = input.chars().collect::<Vec<_>>();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '(' {
            let mut j = i + 1;
            let mut marker = String::new();
            while j < chars.len() && chars[j] != ')' {
                marker.push(chars[j]);
                j += 1;
            }
            if j == chars.len() {
                break; // Malformed marker, treat it as regular characters for the rest of the string
            }
            let parts: Vec<&str> = marker.split('x').collect();
            let repeat_len: usize = parts[0].parse().unwrap();
            let repeat_count: usize = parts[1].parse().unwrap();
            len += repeat_len * repeat_count;
            i = j + 1 + repeat_len;
           
        }
         else {
            len += 1;
            i += 1;
        }
    }
    len
}

fn main() -> io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let trimmed_contents = contents.trim().replace(" ", "");

    let decompressed_length = decompress_len(&trimmed_contents);
    println!("{}", decompressed_length);

    Ok(())
}
