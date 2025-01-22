
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let line = reader.lines().next().unwrap()?;
    let mut stones: Vec<u64> = line
        .split_whitespace()
        .map(|s| u64::from_str(s).unwrap())
        .collect();

    for _ in 0..25 {
        let mut next_stones = Vec::new();
        for &stone in &stones {
            let stone_str = stone.to_string();
            if stone == 0 {
                next_stones.push(1);
            } else if stone_str.len() % 2 == 0 {
                let mid = stone_str.len() / 2;
                let left = u64::from_str(&stone_str[..mid]).unwrap();
                let right = u64::from_str(&stone_str[mid..]).unwrap();
                next_stones.push(left);
                next_stones.push(right);
            } else {
                next_stones.push(stone * 2024);
            }
        }
        stones = next_stones;
    }

    println!("{}", stones.len());
    Ok(())
}
