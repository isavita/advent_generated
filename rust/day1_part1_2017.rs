
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let digits: Vec<u32> = input.trim().chars().map(|c| c.to_digit(10).unwrap()).collect();
    
    let mut sum = 0;
    for i in 0..digits.len() {
        if digits[i] == digits[(i + 1) % digits.len()] {
            sum += digits[i];
        }
    }
    
    println!("{}", sum);
}
