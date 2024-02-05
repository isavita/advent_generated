
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut prev_row = input.trim().to_string();
    let mut safe_count = prev_row.chars().filter(|&c| c == '.').count();

    for _ in 1..400000 {
        let mut new_row = String::new();
        let chars = prev_row.chars().collect::<Vec<char>>();
        
        for i in 0..prev_row.len() {
            let left = if i == 0 { '.' } else { chars[i - 1] };
            let center = chars[i];
            let right = if i == prev_row.len() - 1 { '.' } else { chars[i + 1] };

            if (left == '^' && center == '^' && right == '.') ||
               (center == '^' && right == '^' && left == '.') ||
               (left == '^' && center == '.' && right == '.') ||
               (right == '^' && center == '.' && left == '.') {
                new_row.push('^');
            } else {
                new_row.push('.');
                safe_count += 1;
            }
        }

        prev_row = new_row;
    }

    println!("{}", safe_count);
}
