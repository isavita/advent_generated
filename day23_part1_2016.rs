
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut instructions: Vec<Vec<&str>> = input.lines().map(|line| line.split_whitespace().collect()).collect();
    
    let mut registers = vec![0, 0, 0, 0];
    registers[0] = 7;
    
    let mut pc = 0;
    
    while pc < instructions.len() {
        let parts = &instructions[pc];
        
        match parts[0] {
            "cpy" => {
                let val = if let Ok(val) = parts[1].parse() { val } else { registers[(parts[1].chars().next().unwrap() as u8 - b'a') as usize] };
                let reg = (parts[2].chars().next().unwrap() as u8 - b'a') as usize;
                registers[reg] = val;
            },
            "inc" => {
                let reg = (parts[1].chars().next().unwrap() as u8 - b'a') as usize;
                registers[reg] += 1;
            },
            "dec" => {
                let reg = (parts[1].chars().next().unwrap() as u8 - b'a') as usize;
                registers[reg] -= 1;
            },
            "jnz" => {
                let check = if let Ok(val) = parts[1].parse() { val } else { registers[(parts[1].chars().next().unwrap() as u8 - b'a') as usize] };
                let jump = if let Ok(val) = parts[2].parse() { val } else { registers[(parts[2].chars().next().unwrap() as u8 - b'a') as usize] };
                if check != 0 {
                    pc = (pc as i32 + jump) as usize;
                    continue;
                }
            },
            "tgl" => {
                let toggle = if let Ok(val) = parts[1].parse() { val } else { registers[(parts[1].chars().next().unwrap() as u8 - b'a') as usize] };
                let toggle_idx = (pc as i32 + toggle) as usize;
                if toggle_idx < instructions.len() {
                    match instructions[toggle_idx][0] {
                        "inc" => instructions[toggle_idx][0] = "dec",
                        "dec" => instructions[toggle_idx][0] = "inc",
                        "tgl" => instructions[toggle_idx][0] = "inc",
                        "jnz" => instructions[toggle_idx][0] = "cpy",
                        "cpy" => instructions[toggle_idx][0] = "jnz",
                        _ => (),
                    }
                }
            },
            _ => (),
        }
        
        pc += 1;
    }
    
    println!("{}", registers[0]);
}
