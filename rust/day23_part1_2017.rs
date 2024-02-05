
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut mul_count = 0;
    let mut pointer = 0;
    let mut registers: std::collections::HashMap<String, i32> = std::collections::HashMap::new();
    let mut instructions: Vec<String> = Vec::new();

    for line in reader.lines() {
        instructions.push(line.unwrap());
    }

    while pointer >= 0 && (pointer as usize) < instructions.len() {
        let parts: Vec<&str> = instructions[pointer as usize].split_whitespace().collect();
        let cmd = parts[0];
        let x = parts[1];
        let y = parts[2];

        let get_value = |s: &str| -> i32 {
            if let Ok(v) = s.parse::<i32>() {
                return v;
            }
            *registers.get(s).unwrap_or(&0)
        };

        match cmd {
            "set" => {
                registers.insert(x.to_string(), get_value(y));
            }
            "sub" => {
                *registers.entry(x.to_string()).or_insert(0) -= get_value(y);
            }
            "mul" => {
                *registers.entry(x.to_string()).or_insert(0) *= get_value(y);
                mul_count += 1;
            }
            "jnz" => {
                if get_value(x) != 0 {
                    pointer += get_value(y) - 1;
                }
            }
            _ => {}
        }
        pointer += 1;
    }

    println!("{}", mul_count);
}
