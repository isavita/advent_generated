
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let mut current = input;

    for _ in 0..40 {
        let mut new_current = String::new();
        let mut count = 1;
        let mut prev_char = current.chars().next().unwrap();

        for c in current.chars().skip(1) {
            if c == prev_char {
                count += 1;
            } else {
                new_current.push_str(&count.to_string());
                new_current.push(prev_char);
                count = 1;
                prev_char = c;
            }
        }

        new_current.push_str(&count.to_string());
        new_current.push(prev_char);

        current = new_current;
    }

    println!("{}", current.len());
}
