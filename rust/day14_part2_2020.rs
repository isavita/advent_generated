
use std::collections::HashMap;
use std::fs;

fn apply_mask_v1(value: u64, mask: &str) -> u64 {
    let mut result = value;
    for (index, bit) in mask.chars().rev().enumerate() {
        match bit {
            '0' => result &= !(1 << index),
            '1' => result |= 1 << index,
            _ => (),
        }
    }
    result
}

fn apply_mask_v2(address: u64, mask: &str) -> Vec<u64> {
    let mut addresses = vec![address];
    for (index, bit) in mask.chars().rev().enumerate() {
        match bit {
            '1' => {
                for addr in &mut addresses {
                    *addr |= 1 << index;
                }
            }
            'X' => {
                let mut new_addresses = Vec::new();
                for addr in &addresses {
                    new_addresses.push(addr | 1 << index);
                    new_addresses.push(addr & !(1 << index));
                }
                addresses.extend(new_addresses);
            }
            _ => (),
        }
    }
    addresses
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut memory_v1: HashMap<u64, u64> = HashMap::new();
    let mut memory_v2: HashMap<u64, u64> = HashMap::new();
    let mut mask = "";
    
    for line in input.lines() {
        if line.starts_with("mask") {
            mask = line.split(" = ").last().unwrap();
        } else {
            let parts: Vec<&str> = line.split(|c| c == '[' || c == ']').collect();
            let address = parts[1].parse().unwrap();
            let value = parts[2].split(" = ").last().unwrap().parse().unwrap();
            
            memory_v1.insert(address, apply_mask_v1(value, mask));
            
            for addr in apply_mask_v2(address, mask) {
                memory_v2.insert(addr, value);
            }
        }
    }
    
    let sum_v1: u64 = memory_v1.values().sum();
    let sum_v2: u64 = memory_v2.values().sum();
    
    println!("{}", sum_v1);
    println!("{}", sum_v2);
}

