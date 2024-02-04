
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut count = 0;

    for line in input.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let range: Vec<&str> = parts[0].split("-").collect();
        let min: usize = range[0].parse().unwrap();
        let max: usize = range[1].parse().unwrap();
        let letter = parts[1].chars().next().unwrap();
        let password = parts[2];

        let occurrences = password.matches(letter).count();
        if occurrences >= min && occurrences <= max {
            count += 1;
        }
    }

    println!("{}", count);
}
