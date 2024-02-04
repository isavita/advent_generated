
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    
    let nice_count = input.lines().filter(|line| {
        let mut has_pair = false;
        let mut has_repeat_with_gap = false;

        for i in 0..line.len() - 2 {
            let pair = &line[i..i+2];
            if line[i+2..].contains(pair) {
                has_pair = true;
                break;
            }
        }

        for i in 0..line.len() - 2 {
            if line.chars().nth(i) == line.chars().nth(i + 2) {
                has_repeat_with_gap = true;
                break;
            }
        }

        has_pair && has_repeat_with_gap
    }).count();

    println!("{}", nice_count);
}
