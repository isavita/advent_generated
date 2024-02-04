
use std::fs;

fn reverse_section(arr: &mut [u8], start: usize, length: usize) {
    let n = arr.len();
    for i in 0..length / 2 {
        arr.swap((start + i) % n, (start + length - 1 - i) % n);
    }
}

fn knot_hash(input: &str) -> String {
    let mut lengths: Vec<u8> = input.bytes().collect();
    lengths.extend(&[17, 31, 73, 47, 23]);

    let mut list: Vec<u8> = (0..256).map(|x| x as u8).collect();

    let mut position = 0;
    let mut skip = 0;
    for _ in 0..64 {
        for &length in &lengths {
            reverse_section(&mut list, position, length as usize);
            position = (position + length as usize + skip) % 256;
            skip += 1;
        }
    }

    let dense_hash: Vec<u8> = list.chunks(16)
        .map(|chunk| chunk.iter().fold(0, |acc, &x| acc ^ x))
        .collect();

    let hex_hash: String = dense_hash.iter().map(|x| format!("{:02x}", x)).collect();
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

fn dfs(x: usize, y: usize, grid: &mut Vec<Vec<u8>>) {
    if x >= 128 || y >= 128 || grid[x][y] != 1 {
        return;
    }
    grid[x][y] = 0;
    if x > 0 { dfs(x - 1, y, grid); }
    if x < 127 { dfs(x + 1, y, grid); }
    if y > 0 { dfs(x, y - 1, grid); }
    if y < 127 { dfs(x, y + 1, grid); }
}

fn main() {
    let data = fs::read_to_string("input.txt").expect("File reading error");
    let key_string = data.trim();

    let mut grid = vec![vec![0; 128]; 128];
    let mut total_used = 0;
    let mut regions = 0;

    for i in 0..128 {
        let row_key = format!("{}-{}", key_string, i);
        let hash = knot_hash(&row_key);
        let binary_row = hex_to_binary(&hash);

        for (j, bit) in binary_row.chars().enumerate() {
            if bit == '1' {
                grid[i][j] = 1;
                total_used += 1;
            }
        }
    }

    for i in 0..128 {
        for j in 0..128 {
            if grid[i][j] == 1 {
                regions += 1;
                dfs(i, j, &mut grid);
            }
        }
    }

    println!("{}", regions);
}
