
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let instructions: Vec<&str> = input.trim().split('\n').collect();
    let mut registers = vec![0, 0, 0, 0];
    let mut pc = 0;

    while pc < instructions.len() {
        let parts: Vec<&str> = instructions[pc].split_whitespace().collect();
        match parts[0] {
            "cpy" => {
                let val = match parts[1].parse() {
                    Ok(num) => num,
                    Err(_) => registers[(parts[1].as_bytes()[0] - b'a') as usize],
                };
                registers[(parts[2].as_bytes()[0] - b'a') as usize] = val;
                pc += 1;
            }
            "inc" => {
                let reg = (parts[1].as_bytes()[0] - b'a') as usize;
                registers[reg] += 1;
                pc += 1;
            }
            "dec" => {
                let reg = (parts[1].as_bytes()[0] - b'a') as usize;
                registers[reg] -= 1;
                pc += 1;
            }
            "jnz" => {
                let val = match parts[1].parse() {
                    Ok(num) => num,
                    Err(_) => registers[(parts[1].as_bytes()[0] - b'a') as usize],
                };
                if val != 0 {
                    pc = (pc as i32 + parts[2].parse::<i32>().unwrap()) as usize;
                } else {
                    pc += 1;
                }
            }
            _ => {}
        }
    }

    println!("{}", registers[0]);
}
