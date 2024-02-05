
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let program: Vec<i32> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();

    let mut memory = program.clone();
    let mut ip = 0;

    loop {
        let opcode = memory[ip] % 100;
        let mode1 = (memory[ip] / 100) % 10;
        let mode2 = (memory[ip] / 1000) % 10;

        match opcode {
            1 => {
                let val1 = if mode1 == 0 { memory[memory[ip + 1] as usize] } else { memory[ip + 1] };
                let val2 = if mode2 == 0 { memory[memory[ip + 2] as usize] } else { memory[ip + 2] };
                let dest = memory[ip + 3] as usize;
                memory[dest] = val1 + val2;
                ip += 4;
            }
            2 => {
                let val1 = if mode1 == 0 { memory[memory[ip + 1] as usize] } else { memory[ip + 1] };
                let val2 = if mode2 == 0 { memory[memory[ip + 2] as usize] } else { memory[ip + 2] };
                let dest = memory[ip + 3] as usize;
                memory[dest] = val1 * val2;
                ip += 4;
            }
            3 => {
                let dest = memory[ip + 1] as usize;
                memory[dest] = 1; // Input value
                ip += 2;
            }
            4 => {
                let val = if mode1 == 0 { memory[memory[ip + 1] as usize] } else { memory[ip + 1] };
                println!("{}", val);
                ip += 2;
            }
            99 => break,
            _ => panic!("Invalid opcode"),
        }
    }
}
