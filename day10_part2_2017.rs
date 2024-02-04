
use std::fs::File;
use std::io::{BufReader, prelude::*};
use std::collections::VecDeque;

fn main() {
    let file = File::open("input.txt").expect("File not found");
    let reader = BufReader::new(file);

    let mut input = String::new();
    for line in reader.lines() {
        input = line.unwrap();
    }

    let mut lengths: Vec<i32> = input.chars().map(|c| c as i32).collect();
    lengths.extend(&[17, 31, 73, 47, 23]);

    let mut list: Vec<i32> = (0..256).collect();
    let mut current_position = 0;
    let mut skip_size = 0;

    for _ in 0..64 {
        for &length in &lengths {
            let mut start = current_position as usize;
            let mut end = (current_position + length - 1) as usize;
            while start < end {
                list.swap(start % 256, end % 256);
                start += 1;
                end -= 1;
            }
            current_position = (current_position + length + skip_size) % 256;
            skip_size += 1;
        }
    }

    let mut dense_hash: Vec<u8> = Vec::new();
    for i in (0..256).step_by(16) {
        let xor = list[i..i + 16].iter().fold(0, |acc, &x| acc ^ x);
        dense_hash.push(xor as u8);
    }

    let hex_hash: String = dense_hash.iter().map(|&x| format!("{:02x}", x)).collect();

    println!("{}", hex_hash);
}
