use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut counts = [[0; 2]; 12];

    for line in input.lines() {
        let num = line.trim();
        for (i, c) in num.chars().enumerate() {
            counts[i][c.to_digit(10).unwrap() as usize] += 1;
        }
    }

    let mut gamma_rate = 0;
    let mut epsilon_rate = 0;
    for i in 0..counts.len() {
        if counts[i][0] > counts[i][1] {
            gamma_rate |= 1 << (counts.len() - i - 1);
        } else {
            epsilon_rate |= 1 << (counts.len() - i - 1);
        }
    }

    println!("{}", gamma_rate * epsilon_rate);
}