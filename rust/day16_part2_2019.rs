
use std::fs::File;
use std::io::{BufReader, prelude::*};

fn main() {
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut input = String::new();
    for line in reader.lines() {
        input = line.expect("Unable to read line");
    }

    let mut repeated_input = repeat_input(&input, 10000);

    let offset: usize = input[..7].parse().expect("Unable to parse offset");

    for _ in 0..100 {
        let mut sum = 0;
        for i in (offset..repeated_input.len()).rev() {
            sum += repeated_input[i];
            repeated_input[i] = sum % 10;
        }
    }

    for i in offset..offset+8 {
        print!("{}", repeated_input[i]);
    }
    println!();
}

fn repeat_input(input: &str, times: usize) -> Vec<u32> {
    let mut digits = vec![0; input.len() * times];
    for t in 0..times {
        for (i, r) in input.chars().enumerate() {
            let digit: u32 = r.to_digit(10).expect("Unable to parse digit");
            digits[t * input.len() + i] = digit;
        }
    }
    digits
}
