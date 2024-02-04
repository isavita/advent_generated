
use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let instructions: Vec<&str> = input.trim().lines().collect();

    let mut acc = 0;
    let mut index = 0;
    let mut visited = HashSet::new();

    while visited.insert(index) {
        let parts: Vec<&str> = instructions[index].split_whitespace().collect();
        let value: i32 = parts[1].parse().unwrap();

        match parts[0] {
            "acc" => {
                acc += value;
                index += 1;
            }
            "jmp" => {
                index = (index as i32 + value) as usize;
            }
            _ => {
                index += 1;
            }
        }
    }

    println!("{}", acc);
}
