
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let steps: usize = input.trim().parse().unwrap();

    let mut buffer = vec![0];
    let mut current_pos = 0;

    for i in 1..=2017 {
        current_pos = (current_pos + steps) % buffer.len() + 1;
        buffer.insert(current_pos, i);
    }

    println!("{}", buffer[(current_pos + 1) % buffer.len()]);
}
