
use std::fs;
use std::str::FromStr;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
enum Instruction {
    Acc(i32),
    Jmp(i32),
    Nop(i32),
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split_whitespace().collect();
        if parts.len() != 2 {
            return Err(());
        }

        let op = parts[0];
        let arg = parts[1].parse::<i32>().map_err(|_| ())?;

        match op {
            "acc" => Ok(Instruction::Acc(arg)),
            "jmp" => Ok(Instruction::Jmp(arg)),
            "nop" => Ok(Instruction::Nop(arg)),
            _ => Err(()),
        }
    }
}

fn run_program(program: &[Instruction]) -> Result<i32, i32> {
    let mut acc = 0;
    let mut pc = 0;
    let mut visited = HashSet::new();

    while pc < program.len() {
        if !visited.insert(pc) {
            return Err(acc); // Infinite loop detected
        }

        match program[pc] {
            Instruction::Acc(arg) => {
                acc += arg;
                pc += 1;
            }
            Instruction::Jmp(arg) => {
                pc = (pc as i32 + arg) as usize;
            }
            Instruction::Nop(_) => {
                pc += 1;
            }
        }
    }
    Ok(acc) // Program terminated
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let original_program: Vec<Instruction> = contents.lines()
        .map(|line| Instruction::from_str(line).expect("Invalid Instruction"))
        .collect();

    // Part 1
    match run_program(&original_program) {
        Err(acc) => println!("Part 1: Accumulator before loop: {}", acc),
        Ok(_) => unreachable!("Program should not terminate in part 1"),
    }

    // Part 2
    for i in 0..original_program.len() {
        let mut modified_program = original_program.clone();
        match modified_program[i] {
            Instruction::Jmp(arg) => modified_program[i] = Instruction::Nop(arg),
            Instruction::Nop(arg) => modified_program[i] = Instruction::Jmp(arg),
            _ => continue, // Skip acc instructions
        }

        match run_program(&modified_program) {
            Ok(acc) => {
                println!("Part 2: Accumulator after termination: {}", acc);
                return;
            }
            Err(_) => continue,
        }
    }
}
