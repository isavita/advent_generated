
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let instructions: Vec<&str> = input.trim().split('\n').collect();

    let mut registers = vec![0, 0, 1, 0];
    let mut pc = 0;

    while pc < instructions.len() {
        let parts: Vec<&str> = instructions[pc].split(' ').collect();
        match parts[0] {
            "cpy" => {
                let val = match parts[1].parse() {
                    Ok(x) => x,
                    Err(_) => registers[(parts[1].as_bytes()[0] - b'a') as usize],
                };
                let reg = (parts[2].as_bytes()[0] - b'a') as usize;
                registers[reg] = val;
            }
            "inc" => {
                let reg = (parts[1].as_bytes()[0] - b'a') as usize;
                registers[reg] += 1;
            }
            "dec" => {
                let reg = (parts[1].as_bytes()[0] - b'a') as usize;
                registers[reg] -= 1;
            }
            "jnz" => {
                let val = match parts[1].parse() {
                    Ok(x) => x,
                    Err(_) => registers[(parts[1].as_bytes()[0] - b'a') as usize],
                };
                if val != 0 {
                    let offset = parts[2].parse::<i32>().unwrap();
                    pc = (pc as i32 + offset - 1) as usize;
                }
            }
            _ => {}
        }
        pc += 1;
    }

    println!("{}", registers[0]);
}
