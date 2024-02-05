
use std::fs;

fn reverse_section(arr: &mut [u8], start: usize, length: usize) {
    let n = arr.len();
    for i in 0..length / 2 {
        arr.swap((start + i) % n, (start + length - 1 - i) % n);
    }
}

fn knot_hash(input: &str) -> String {
    let mut lengths: Vec<usize> = input.bytes().map(|b| b as usize).collect();
    lengths.extend(&[17, 31, 73, 47, 23]);

    let mut list: Vec<u8> = (0..256).map(|i| i as u8).collect();

    let (mut position, mut skip) = (0, 0);
    for _ in 0..64 {
        for &length in &lengths {
            reverse_section(&mut list, position, length);
            position = (position + length + skip) % 256;
            skip += 1;
        }
    }

    let dense_hash: Vec<u8> = (0..16)
        .map(|i| list[i * 16..(i + 1) * 16].iter().fold(0, |acc, &x| acc ^ x))
        .collect();

    let hex_hash: String = dense_hash.iter().map(|&x| format!("{:02x}", x)).collect();
    hex_hash
}

fn hex_to_binary(hex_str: &str) -> String {
    let mut binary_str = String::new();
    for hex_digit in hex_str.chars() {
        let val = u8::from_str_radix(&hex_digit.to_string(), 16).unwrap();
        binary_str += &format!("{:04b}", val);
    }
    binary_str
}

fn main() {
    let data = fs::read_to_string("input.txt").expect("File reading error");

    let key_string = data.trim();
    let mut total_used = 0;

    for i in 0..128 {
        let row_key = format!("{}-{}", key_string, i);
        let hash = knot_hash(&row_key);
        let binary_row = hex_to_binary(&hash);

        total_used += binary_row.chars().filter(|&c| c == '1').count();
    }

    println!("{}", total_used);
}
