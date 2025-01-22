
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
enum Instruction {
    Hlf(char),
    Tpl(char),
    Inc(char),
    Jmp(i32),
    Jie(char, i32),
    Jio(char, i32),
}

fn parse_instruction(line: &str) -> Instruction {
    let parts: Vec<&str> = line.split_whitespace().collect();
    match parts[0] {
        "hlf" => Instruction::Hlf(parts[1].chars().next().unwrap()),
        "tpl" => Instruction::Tpl(parts[1].chars().next().unwrap()),
        "inc" => Instruction::Inc(parts[1].chars().next().unwrap()),
        "jmp" => Instruction::Jmp(parts[1].parse().unwrap()),
        "jie" => Instruction::Jie(
            parts[1].chars().next().unwrap(),
            parts[2].trim_start_matches(',').parse().unwrap(),
        ),
        "jio" => Instruction::Jio(
            parts[1].chars().next().unwrap(),
            parts[2].trim_start_matches(',').parse().unwrap(),
        ),
        _ => panic!("Invalid instruction"),
    }
}

fn run_program(instructions: &[Instruction], initial_a: u32) -> u32 {
    let mut registers = vec![initial_a, 0];
    let mut pc = 0;

    while pc >= 0 && pc < instructions.len() as i32 {
        match &instructions[pc as usize] {
            Instruction::Hlf(r) => {
                registers[(*r as u8 - b'a') as usize] /= 2;
                pc += 1;
            }
            Instruction::Tpl(r) => {
                registers[(*r as u8 - b'a') as usize] *= 3;
                pc += 1;
            }
            Instruction::Inc(r) => {
                registers[(*r as u8 - b'a') as usize] += 1;
                pc += 1;
            }
            Instruction::Jmp(offset) => {
                pc += offset;
            }
            Instruction::Jie(r, offset) => {
                if registers[(*r as u8 - b'a') as usize] % 2 == 0 {
                    pc += offset;
                } else {
                    pc += 1;
                }
            }
            Instruction::Jio(r, offset) => {
                if registers[(*r as u8 - b'a') as usize] == 1 {
                    pc += offset;
                } else {
                    pc += 1;
                }
            }
        }
    }

    registers[1]
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);

    let instructions: Vec<Instruction> = reader
        .lines()
        .map(|line| parse_instruction(&line.unwrap()))
        .collect();

    let result_part1 = run_program(&instructions, 0);
    println!("Part 1: {}", result_part1);

    let result_part2 = run_program(&instructions, 1);
    println!("Part 2: {}", result_part2);
}
