
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");
    let sum = calculate_sum(&input);
    println!("{}", sum);
}

fn calculate_sum(input: &str) -> i64 {
    let mut sum = 0;
    let mut current_number = String::new();
    let mut is_negative = false;

    for c in input.chars() {
        match c {
            '-' => {
                is_negative = true;
            }
            '0'..='9' => {
                current_number.push(c);
            }
            _ => {
                if !current_number.is_empty() {
                    let num = current_number.parse::<i64>().unwrap();
                    sum += if is_negative { -num } else { num };
                    current_number.clear();
                    is_negative = false;
                }
            }
        }
    }

    // Handle any remaining number at the end of the string
     if !current_number.is_empty() {
        let num = current_number.parse::<i64>().unwrap();
        sum += if is_negative { -num } else { num };
    }
    sum
}
