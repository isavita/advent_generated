
use std::fs::read_to_string;

fn main() {
    let total: u32 = read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|bank| {
            for &d1 in b"9876543210" {
                if let Some(pos) = bank.as_bytes().iter().position(|&c| c == d1) {
                    let rest = &bank.as_bytes()[pos + 1..];
                    let max_d2 = rest.iter().filter_map(|&b| (b as char).to_digit(10)).max();
                    if let Some(d2) = max_d2 {
                        return (d1 - b'0') as u32 * 10 + d2;
                    }
                }
            }
            0
        })
        .sum();
    println!("Total output joltage: {}", total);
}
