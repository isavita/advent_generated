
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let instructions: Vec<&str> = input.trim().split('\n').collect();
    let mut a = 0;
    let mut b = 0;
    let mut i = 0;

    while i < instructions.len() {
        let parts: Vec<&str> = instructions[i].split(' ').collect();
        match parts[0] {
            "hlf" => {
                match parts[1] {
                    "a" => a /= 2,
                    "b" => b /= 2,
                    _ => (),
                }
                i += 1;
            }
            "tpl" => {
                match parts[1] {
                    "a" => a *= 3,
                    "b" => b *= 3,
                    _ => (),
                }
                i += 1;
            }
            "inc" => {
                match parts[1] {
                    "a" => a += 1,
                    "b" => b += 1,
                    _ => (),
                }
                i += 1;
            }
            "jmp" => {
                let offset: i32 = parts[1].parse().unwrap();
                i = (i as i32 + offset) as usize;
            }
            "jie" => {
                let reg = parts[1].trim_end_matches(',').to_string();
                let offset: i32 = parts[2].parse().unwrap();
                if (reg == "a" && a % 2 == 0) || (reg == "b" && b % 2 == 0) {
                    i = (i as i32 + offset) as usize;
                } else {
                    i += 1;
                }
            }
            "jio" => {
                let reg = parts[1].trim_end_matches(',').to_string();
                let offset: i32 = parts[2].parse().unwrap();
                if (reg == "a" && a == 1) || (reg == "b" && b == 1) {
                    i = (i as i32 + offset) as usize;
                } else {
                    i += 1;
                }
            }
            _ => (),
        }
    }

    println!("{}", b);
}
