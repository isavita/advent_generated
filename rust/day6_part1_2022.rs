
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    
    let mut count = 0;
    let mut chars: Vec<char> = Vec::new();
    
    for c in input.chars() {
        count += 1;
        chars.push(c);
        
        if chars.len() >= 4 {
            let last_four: Vec<char> = chars.iter().rev().take(4).cloned().collect();
            let unique_chars: Vec<char> = last_four.iter().cloned().collect();
            
            if unique_chars.iter().collect::<std::collections::HashSet<_>>().len() == 4 {
                println!("{}", count);
                break;
            }
        }
    }
}
