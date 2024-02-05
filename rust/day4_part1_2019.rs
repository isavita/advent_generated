
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let range: Vec<u32> = input.trim().split("-").map(|x| x.parse().unwrap()).collect();
    
    let mut count = 0;
    for num in range[0]..=range[1] {
        let digits: Vec<u32> = num.to_string().chars().map(|c| c.to_digit(10).unwrap()).collect();
        let mut has_adjacent = false;
        let mut never_decrease = true;
        
        for i in 1..digits.len() {
            if digits[i] == digits[i - 1] {
                has_adjacent = true;
            }
            if digits[i] < digits[i - 1] {
                never_decrease = false;
                break;
            }
        }
        
        if has_adjacent && never_decrease {
            count += 1;
        }
    }
    
    println!("{}", count);
}
