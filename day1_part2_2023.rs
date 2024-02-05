
use std::fs::File;
use std::io::{BufReader, prelude::*};

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut sum = 0;
    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        let (first_digit, last_digit) = find_first_and_last_digit(&line);
        sum += 10 * first_digit + last_digit;
    }

    println!("{}", sum);
}

fn find_first_and_last_digit(line: &str) -> (i32, i32) {
    let digits = vec!["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

    let mut first_digit = 0;
    let mut last_digit = 0;
    for (i, char) in line.chars().enumerate() {
        let digit_str = char.to_string();
        if let Ok(digit) = digit_str.parse::<i32>() {
            if first_digit == 0 {
                first_digit = digit;
            }
            last_digit = digit;
        } else {
            for (j, digit) in digits.iter().enumerate() {
                if line[i..].starts_with(digit) {
                    if first_digit == 0 {
                        first_digit = j as i32;
                    }
                    last_digit = j as i32;
                    break;
                }
            }
        }
    }

    (first_digit, last_digit)
}
