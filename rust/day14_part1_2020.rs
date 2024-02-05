
use std::collections::HashMap;
use std::fs;

fn apply_mask(mask: &str, value: u64) -> u64 {
    let mut result = value;
    for (i, c) in mask.chars().enumerate() {
        match c {
            '1' => result |= 1 << (35 - i),
            '0' => result &= !(1 << (35 - i)),
            _ => continue,
        }
    }
    result
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut mask = "";
    let mut memory: HashMap<u64, u64> = HashMap::new();

    for line in input.lines() {
        let parts: Vec<&str> = line.split(" = ").collect();
        if parts[0] == "mask" {
            mask = parts[1];
        } else {
            let address = parts[0].trim_start_matches("mem[").trim_end_matches("]").parse().unwrap();
            let value = parts[1].parse().unwrap();
            memory.insert(address, apply_mask(mask, value));
        }
    }

    let sum: u64 = memory.values().sum();
    println!("{}", sum);
}
