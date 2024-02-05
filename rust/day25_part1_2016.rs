
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut instructions: Vec<String> = Vec::new();
    for line in reader.lines() {
        instructions.push(line.unwrap());
    }

    for a in 1.. {
        if produces_clock_signal(a, &instructions) {
            println!("{}", a);
            break;
        }
    }
}

fn produces_clock_signal(a: i32, instructions: &Vec<String>) -> bool {
    let mut registers = vec![("a", a), ("b", 0), ("c", 0), ("d", 0)]
        .iter()
        .cloned()
        .collect::<std::collections::HashMap<_, _>>();
    let mut last_output = 0;
    let mut output_count = 0;

    let mut i = 0;
    while i < instructions.len() {
        let parts: Vec<&str> = instructions[i].split_whitespace().collect();
        match parts[0] {
            "cpy" => {
                let val = get_value(parts[1], &registers);
                *registers.get_mut(parts[2]).unwrap() = val;
            }
            "inc" => {
                *registers.get_mut(parts[1]).unwrap() += 1;
            }
            "dec" => {
                *registers.get_mut(parts[1]).unwrap() -= 1;
            }
            "jnz" => {
                let val = get_value(parts[1], &registers);
                if val != 0 {
                    let jump: i32 = parts[2].parse().unwrap();
                    i = (i as i32 + jump - 1) as usize;
                }
            }
            "out" => {
                let val = get_value(parts[1], &registers);
                if val != 0 && val != 1 {
                    return false;
                }
                if output_count > 0 && val == last_output {
                    return false;
                }
                last_output = val;
                output_count += 1;
                if output_count > 50 {
                    return true;
                }
            }
            _ => {}
        }
        i += 1;
    }
    false
}

fn get_value(s: &str, registers: &std::collections::HashMap<&str, i32>) -> i32 {
    s.parse().unwrap_or_else(|_| *registers.get(s).unwrap())
}
