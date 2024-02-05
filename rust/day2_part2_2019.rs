
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut program: Vec<usize> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();
    
    // Part One
    program[1] = 12;
    program[2] = 2;
    let result = run_program(&mut program.clone());
    println!("{}", result);
    
    // Part Two
    for noun in 0..=99 {
        for verb in 0..=99 {
            let mut program_copy = program.clone();
            program_copy[1] = noun;
            program_copy[2] = verb;
            let output = run_program(&mut program_copy);
            if output == 19690720 {
                println!("{}", 100 * noun + verb);
                return;
            }
        }
    }
}

fn run_program(program: &mut Vec<usize>) -> usize {
    let mut ip = 0;
    loop {
        match program[ip] {
            1 => {
                let a = program[ip + 1];
                let b = program[ip + 2];
                let dest = program[ip + 3];
                program[dest] = program[a] + program[b];
                ip += 4;
            }
            2 => {
                let a = program[ip + 1];
                let b = program[ip + 2];
                let dest = program[ip + 3];
                program[dest] = program[a] * program[b];
                ip += 4;
            }
            99 => break,
            _ => panic!("Invalid opcode"),
        }
    }
    program[0]
}
