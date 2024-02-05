use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let steps: usize = input.trim().parse().expect("Invalid input");

    let mut buffer = vec![0];
    let mut pos = 0;

    for i in 1..=2017 {
        pos = (pos + steps) % buffer.len() + 1;
        buffer.insert(pos, i);
    }

    println!("{}", buffer[(pos + 1) % buffer.len()]);

    let mut after_zero = 0;
    let mut len = 1;
    pos = 0;

    for i in 1..=50000000 {
        pos = (pos + steps) % len + 1;
        len += 1;
        if pos == 1 {
            after_zero = i;
        }
    }

    println!("{}", after_zero);
}