
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let instructions: Vec<&str> = input.trim().split('\n').collect();
    
    let mut registers: std::collections::HashMap<char, i64> = std::collections::HashMap::new();
    let mut last_sound = 0;
    let mut pc: i64 = 0;
    
    while pc >= 0 && (pc as usize) < instructions.len() {
        let parts: Vec<&str> = instructions[pc as usize].split_whitespace().collect();
        let reg = parts[1].chars().next().unwrap();
        let val = if parts.len() == 3 {
            match parts[2].parse::<i64>() {
                Ok(num) => num,
                Err(_) => *registers.get(&parts[2].chars().next().unwrap()).unwrap_or(&0),
            }
        } else {
            0
        };
        
        match parts[0] {
            "snd" => last_sound = *registers.get(&reg).unwrap_or(&0),
            "set" => { registers.insert(reg, val); },
            "add" => { *registers.entry(reg).or_insert(0) += val; },
            "mul" => { *registers.entry(reg).or_insert(0) *= val; },
            "mod" => { *registers.entry(reg).or_insert(0) %= val; },
            "rcv" => {
                if *registers.get(&reg).unwrap_or(&0) != 0 {
                    println!("{}", last_sound);
                    break;
                }
            },
            "jgz" => {
                if *registers.get(&reg).unwrap_or(&0) > 0 {
                    pc += val - 1;
                }
            },
            _ => {}
        }
        
        pc += 1;
    }
}
