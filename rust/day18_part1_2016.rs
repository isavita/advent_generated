
use std::fs;

fn is_trap(left: char, center: char, right: char) -> char {
    if (left == '^' && center == '^' && right == '.') ||
       (center == '^' && right == '^' && left == '.') ||
       (left == '^' && center == '.' && right == '.') ||
       (right == '^' && center == '.' && left == '.') {
        '^'
    } else {
        '.'
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut current_row: Vec<char> = input.trim().chars().collect();
    let mut safe_tiles = current_row.iter().filter(|&c| *c == '.').count();

    for _ in 1..40 {
        let mut next_row: Vec<char> = Vec::new();
        next_row.push(is_trap('.', current_row[0], current_row[1]));
        for i in 1..current_row.len() - 1 {
            next_row.push(is_trap(current_row[i - 1], current_row[i], current_row[i + 1]));
        }
        next_row.push(is_trap(current_row[current_row.len() - 2], current_row[current_row.len() - 1], '.'));
        
        safe_tiles += next_row.iter().filter(|&c| *c == '.').count();
        current_row = next_row;
    }

    println!("{}", safe_tiles);
}
