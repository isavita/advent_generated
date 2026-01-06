
use std::fs;

fn main() {
    let data = fs::read_to_string("input.txt").unwrap();
    let mut pos = 50i32;
    let mut zeros = 0u32;
    for line in data.lines().map(str::trim).filter(|s| !s.is_empty()) {
        let amount: i32 = line[1..].parse().unwrap();
        pos = match line.as_bytes()[0] {
            b'R' => pos + amount,
            _ => pos - amount,
        }.rem_euclid(100);
        zeros += (pos == 0) as u32;
    }
    println!("The password is: {}", zeros);
}
