use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut adapters: Vec<i32> = Vec::new();
    for line in reader.lines() {
        let line = line?;
        adapters.push(line.parse().unwrap());
    }
    adapters.push(0);
    adapters.sort_unstable();
    adapters.push(adapters[adapters.len() - 1] + 3);
    let mut ways = HashMap::new();
    ways.insert(0, 1);
    for i in 1..adapters.len() {
        let current_joltage = adapters[i];
        let mut count: u64 = 0; // Use u64 to avoid overflow
        for &diff in &[1, 2, 3] {
            if let Some(&value) = ways.get(&(current_joltage - diff)) {
                count += value;
            }
        }
        ways.insert(current_joltage, count);
    }
    println!("{}", ways[&adapters[adapters.len() - 1]]);
    Ok(())
}