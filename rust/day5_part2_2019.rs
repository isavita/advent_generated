
use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let program: Vec<i32> = contents.trim().split(',')
        .map(|x| x.parse().unwrap())
        .collect();

    let mut program = program.clone();

    let mut i = 0;
    while i < program.len() {
        let opcode = program[i] % 100;
        let mode1 = (program[i] / 100) % 10;
        let mode2 = (program[i] / 1000) % 10;
        let mode3 = (program[i] / 10000) % 10;

        match opcode {
            1 => {
                let val1 = if mode1 == 0 { program[program[i + 1] as usize] } else { program[i + 1] };
                let val2 = if mode2 == 0 { program[program[i + 2] as usize] } else { program[i + 2] };
                let pos = program[i + 3] as usize;
                program[pos] = val1 + val2;
                i += 4;
            },
            2 => {
                let val1 = if mode1 == 0 { program[program[i + 1] as usize] } else { program[i + 1] };
                let val2 = if mode2 == 0 { program[program[i + 2] as usize] } else { program[i + 2] };
                let pos = program[i + 3] as usize;
                program[pos] = val1 * val2;
                i += 4;
            },
            3 => {
                let pos = program[i + 1] as usize;
                program[pos] = 5; // Input value for system ID 5
                i += 2;
            },
            4 => {
                let val = if mode1 == 0 { program[program[i + 1] as usize] } else { program[i + 1] };
                println!("{}", val);
                i += 2;
            },
            5 => {
                let val1 = if mode1 == 0 { program[program[i + 1] as usize] } else { program[i + 1] };
                let val2 = if mode2 == 0 { program[program[i + 2] as usize] } else { program[i + 2] };
                if val1 != 0 {
                    i = val2 as usize;
                } else {
                    i += 3;
                }
            },
            6 => {
                let val1 = if mode1 == 0 { program[program[i + 1] as usize] } else { program[i + 1] };
                let val2 = if mode2 == 0 { program[program[i + 2] as usize] } else { program[i + 2] };
                if val1 == 0 {
                    i = val2 as usize;
                } else {
                    i += 3;
                }
            },
            7 => {
                let val1 = if mode1 == 0 { program[program[i + 1] as usize] } else { program[i + 1] };
                let val2 = if mode2 == 0 { program[program[i + 2] as usize] } else { program[i + 2] };
                let pos = program[i + 3] as usize;
                program[pos] = if val1 < val2 { 1 } else { 0 };
                i += 4;
            },
            8 => {
                let val1 = if mode1 == 0 { program[program[i + 1] as usize] } else { program[i + 1] };
                let val2 = if mode2 == 0 { program[program[i + 2] as usize] } else { program[i + 2] };
                let pos = program[i + 3] as usize;
                program[pos] = if val1 == val2 { 1 } else { 0 };
                i += 4;
            },
            99 => {
                break;
            },
            _ => panic!("Invalid opcode"),
        }
    }
}
