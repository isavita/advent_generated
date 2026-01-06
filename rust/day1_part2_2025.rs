
use std::fs::File;
use std::io::{BufRead, BufReader};

fn floor_div(a: i32, b: i32) -> i32 {
    let q = a / b;
    if a < 0 && a % b != 0 { q - 1 } else { q }
}

fn main() {
    const DIAL_SIZE: i32 = 100;
    let mut current_pos = 50;
    let mut total_zero_hits = 0i64;

    let file = File::open("input.txt").unwrap();
    for line in BufReader::new(file).lines().flatten() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let (dir, num) = line.split_at(1);
        let amount: i32 = num.parse().unwrap();
        match dir {
            "R" => {
                total_zero_hits += ((current_pos + amount) / DIAL_SIZE) as i64;
                current_pos = (current_pos + amount) % DIAL_SIZE;
            }
            "L" => {
                total_zero_hits +=
                    (floor_div(current_pos - 1, DIAL_SIZE) - floor_div(current_pos - amount - 1, DIAL_SIZE)) as i64;
                current_pos = (current_pos - amount) % DIAL_SIZE;
                if current_pos < 0 {
                    current_pos += DIAL_SIZE;
                }
            }
            _ => panic!("Unknown direction"),
        }
    }
    println!("The password is: {}", total_zero_hits);
}
