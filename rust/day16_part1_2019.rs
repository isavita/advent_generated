
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut digits: Vec<i32> = input.chars().map(|c| c.to_digit(10).unwrap() as i32).collect();

    for _ in 0..100 {
        digits = apply_fft(&digits);
    }

    for i in 0..8 {
        print!("{}", digits[i]);
    }
    println!();
}

fn apply_fft(input: &Vec<i32>) -> Vec<i32> {
    let base_pattern = vec![0, 1, 0, -1];
    let mut output = vec![0; input.len()];

    for i in 0..input.len() {
        let mut sum = 0;
        for j in 0..input.len() {
            let pattern_value = base_pattern[((j+1)/(i+1)) % base_pattern.len()];
            sum += input[j] * pattern_value;
        }
        output[i] = sum.abs() % 10;
    }

    output
}
