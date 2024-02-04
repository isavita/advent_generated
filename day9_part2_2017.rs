
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut score = 0;
    let mut total_score = 0;
    let mut garbage = false;
    let mut ignore_next = false;
    let mut garbage_count = 0;

    for c in input.chars() {
        if ignore_next {
            ignore_next = false;
        } else {
            match c {
                '!' => ignore_next = true,
                '<' => {
                    if garbage {
                        garbage_count += 1;
                    }
                    garbage = true;
                }
                '>' => garbage = false,
                '{' => {
                    if !garbage {
                        score += 1;
                        total_score += score;
                    } else {
                        garbage_count += 1;
                    }
                }
                '}' => {
                    if !garbage {
                        score -= 1;
                    } else {
                        garbage_count += 1;
                    }
                }
                _ => {
                    if garbage {
                        garbage_count += 1;
                    }
                }
            }
        }
    }

    println!("{}", total_score);
    println!("{}", garbage_count);
}
