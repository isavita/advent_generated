
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut blocked_ranges: Vec<(u32, u32)> = Vec::new();

    for line in input.trim().lines() {
        let parts: Vec<&str> = line.split("-").collect();
        let start: u32 = parts[0].parse().unwrap();
        let end: u32 = parts[1].parse().unwrap();
        blocked_ranges.push((start, end));
    }

    blocked_ranges.sort();

    let mut lowest_ip = 0;

    for (start, end) in blocked_ranges {
        if lowest_ip < start {
            break;
        }
        lowest_ip = end + 1;
    }

    println!("{}", lowest_ip);
}
