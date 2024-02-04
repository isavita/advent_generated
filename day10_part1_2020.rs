
use std::fs;

fn main() {
    let mut input: Vec<i32> = fs::read_to_string("input.txt")
        .expect("Unable to read file")
        .lines()
        .map(|line| line.parse().unwrap())
        .collect();

    input.sort();
    let mut one_jolt_diff = 0;
    let mut three_jolt_diff = 1; // Start with 1 for the built-in adapter

    let mut prev = 0;
    for &num in &input {
        match num - prev {
            1 => one_jolt_diff += 1,
            3 => three_jolt_diff += 1,
            _ => (),
        }
        prev = num;
    }

    println!("{}", one_jolt_diff * three_jolt_diff);
}
