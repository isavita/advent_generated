
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut program: Vec<usize> = input.trim().split(',').map(|x| x.parse().unwrap()).collect();

    program[1] = 12;
    program[2] = 2;

    let mut pos = 0;
    loop {
        match program[pos] {
            1 => {
                let a = program[program[pos + 1]];
                let b = program[program[pos + 2]];
                let dest = program[pos + 3];
                program[dest] = a + b;
                pos += 4;
            }
            2 => {
                let a = program[program[pos + 1]];
                let b = program[program[pos + 2]];
                let dest = program[pos + 3];
                program[dest] = a * b;
                pos += 4;
            }
            99 => break,
            _ => panic!("Unknown opcode"),
        }
    }

    println!("{}", program[0]);
}
