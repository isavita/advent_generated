
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let sum: u64 = input.split(',')
                        .map(|step| {
                            let mut current_value = 0;
                            for c in step.chars() {
                                let ascii_code = c as u64;
                                current_value += ascii_code;
                                current_value *= 17;
                                current_value %= 256;
                            }
                            current_value
                        })
                        .sum();
    
    println!("{}", sum);
}
