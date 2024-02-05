
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    
    let mut count = 0;
    for (i, window) in input.chars().collect::<Vec<char>>().windows(14).enumerate() {
        let unique_chars: Vec<char> = window.iter().cloned().collect();
        if unique_chars.iter().copied().collect::<std::collections::HashSet<char>>().len() == 14 {
            count = i + 14;
            break;
        }
    }
    
    println!("{}", count);
}
