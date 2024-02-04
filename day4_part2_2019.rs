
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let range: Vec<u32> = input.trim().split("-").map(|x| x.parse().unwrap()).collect();
    
    let mut count = 0;
    let mut count_part_two = 0;

    for i in range[0]..=range[1] {
        let password = i.to_string();
        let mut has_adjacent = false;
        let mut has_adjacent_part_two = false;
        let mut never_decreases = true;

        let mut current_run = 1;

        for j in 1..password.len() {
            if password.chars().nth(j).unwrap() == password.chars().nth(j - 1).unwrap() {
                current_run += 1;
            } else {
                if current_run == 2 {
                    has_adjacent_part_two = true;
                }
                if current_run >= 2 {
                    has_adjacent = true;
                }
                current_run = 1;
            }

            if password.chars().nth(j).unwrap() < password.chars().nth(j - 1).unwrap() {
                never_decreases = false;
                break;
            }
        }

        if current_run == 2 {
            has_adjacent_part_two = true;
        }
        if current_run >= 2 {
            has_adjacent = true;
        }

        if has_adjacent && never_decreases {
            count += 1;
        }

        if has_adjacent_part_two && never_decreases {
            count_part_two += 1;
        }
    }

    println!("{}", count);
    println!("{}", count_part_two);
}
