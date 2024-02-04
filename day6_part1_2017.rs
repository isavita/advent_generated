use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut banks: Vec<i32> = input.trim().split_whitespace().map(|x| x.parse().unwrap()).collect();
    let mut seen_configs = HashSet::new();
    let mut cycles = 0;

    while seen_configs.insert(banks.clone()) {
        let max_blocks = *banks.iter().max().unwrap();
        let max_index = banks.iter().position(|&x| x == max_blocks).unwrap();
        let mut blocks_to_redistribute = banks[max_index];
        banks[max_index] = 0;
        let mut current_index = (max_index + 1) % banks.len();

        while blocks_to_redistribute > 0 {
            banks[current_index] += 1;
            blocks_to_redistribute -= 1;
            current_index = (current_index + 1) % banks.len();
        }

        cycles += 1;
    }

    println!("{}", cycles);
}