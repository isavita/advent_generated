
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut score = 0;
    let mut level = 0;
    let mut in_garbage = false;
    let mut ignore_next = false;

    for c in input.chars() {
        if ignore_next {
            ignore_next = false;
        } else {
            match c {
                '{' => {
                    if !in_garbage {
                        level += 1;
                        score += level;
                    }
                }
                '}' => {
                    if !in_garbage {
                        level -= 1;
                    }
                }
                '<' => {
                    in_garbage = true;
                }
                '>' => {
                    in_garbage = false;
                }
                '!' => {
                    ignore_next = true;
                }
                _ => {}
            }
        }
    }

    println!("{}", score);
}
