
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Debug)]
enum Instruction {
    Cpy(Arg, Arg),
    Inc(Arg),
    Dec(Arg),
    Jnz(Arg, Arg),
    Tgl(Arg),
    Mul(Arg, Arg, Arg),
    Nop,
}

#[derive(Clone, Debug)]
enum Arg {
    Register(char),
    Value(i32),
}

fn parse_arg(s: &str) -> Arg {
    if let Ok(val) = s.parse::<i32>() {
        Arg::Value(val)
    } else {
        Arg::Register(s.chars().next().unwrap())
    }
}

fn parse_instructions(filename: &str) -> Vec<Instruction> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut instructions = Vec::new();
    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split_whitespace().collect();
        let instruction = match parts[0] {
            "cpy" => Instruction::Cpy(parse_arg(parts[1]), parse_arg(parts[2])),
            "inc" => Instruction::Inc(parse_arg(parts[1])),
            "dec" => Instruction::Dec(parse_arg(parts[1])),
            "jnz" => Instruction::Jnz(parse_arg(parts[1]), parse_arg(parts[2])),
            "tgl" => Instruction::Tgl(parse_arg(parts[1])),
            _ => panic!("Unknown instruction: {}", parts[0]),
        };
        instructions.push(instruction);
    }
    
    // Optimization pass (identifying multiplication loops)
    let mut optimized_instructions = Vec::new();
    let mut i = 0;
    while i < instructions.len() {
        if i + 5 < instructions.len() {
            if let (
                Instruction::Inc(Arg::Register(a1)),
                Instruction::Dec(Arg::Register(b1)),
                Instruction::Jnz(Arg::Register(b2), Arg::Value(-2)),
                Instruction::Dec(Arg::Register(c1)),
                Instruction::Jnz(Arg::Register(c2), Arg::Value(-5)),
            ) = (
                &instructions[i],
                &instructions[i+1],
                &instructions[i+2],
                &instructions[i+3],
                &instructions[i+4],
            ) {
                if b1 == b2 && c1 == c2 {
                    optimized_instructions.push(Instruction::Mul(
                        Arg::Register(*b1), 
                        Arg::Register(*c1), 
                        Arg::Register(*a1)
                    ));
                    optimized_instructions.push(Instruction::Cpy(Arg::Value(0), Arg::Register(*b1)));
                    optimized_instructions.push(Instruction::Cpy(Arg::Value(0), Arg::Register(*c1)));
                    optimized_instructions.push(Instruction::Nop);
                    optimized_instructions.push(Instruction::Nop);
                    i += 5;
                    continue;
                }
            }
        }
        optimized_instructions.push(instructions[i].clone());
        i += 1;
    }
    
    optimized_instructions
}

fn get_value(registers: &mut [i32; 4], arg: &Arg) -> i32 {
    match arg {
        Arg::Register(c) => registers[(*c as u8 - b'a') as usize],
        Arg::Value(val) => *val,
    }
}

fn set_value(registers: &mut [i32; 4], arg: &Arg, val: i32) {
    match arg {
        Arg::Register(c) => registers[(*c as u8 - b'a') as usize] = val,
        Arg::Value(_) => {} // Ignore writes to values
    }
}

fn execute_instructions(instructions: &mut Vec<Instruction>, initial_a: i32) -> i32 {
    let mut registers = [0; 4];
    registers[0] = initial_a;
    let mut pc = 0;
    while pc < instructions.len() {
        match &instructions[pc] {
            Instruction::Cpy(src, dest) => {
                let val = get_value(&mut registers, src);
                set_value(&mut registers, dest, val);
            }
            Instruction::Inc(reg) => {
                let val = get_value(&mut registers, reg);
                set_value(&mut registers, reg, val + 1);
            }
            Instruction::Dec(reg) => {
                let val = get_value(&mut registers, reg);
                set_value(&mut registers, reg, val - 1);
            }
            Instruction::Jnz(cond, offset) => {
                let val = get_value(&mut registers, cond);
                if val != 0 {
                    pc = (pc as i32 + get_value(&mut registers, offset) - 1) as usize;
                }
            }
            Instruction::Tgl(offset) => {
                let target_pc = (pc as i32 + get_value(&mut registers, offset)) as usize;
                if target_pc < instructions.len() {
                    instructions[target_pc] = match &instructions[target_pc] {
                        Instruction::Inc(x) => Instruction::Dec(x.clone()),
                        Instruction::Dec(x) => Instruction::Inc(x.clone()),
                        Instruction::Tgl(x) => Instruction::Inc(x.clone()),
                        Instruction::Jnz(x, y) => Instruction::Cpy(x.clone(), y.clone()),
                        Instruction::Cpy(x, y) => Instruction::Jnz(x.clone(), y.clone()),
                        Instruction::Mul(_, _, _) => instructions[target_pc].clone(),
                        Instruction::Nop => Instruction::Nop,
                    };
                }
            }
            Instruction::Mul(x, y, dest) => {
                let val = get_value(&mut registers, x) * get_value(&mut registers, y) + get_value(&mut registers, dest);
                set_value(&mut registers, dest, val);
            }
            Instruction::Nop => {},
        }
        pc += 1;
    }
    registers[0]
}

fn main() {
    let mut instructions = parse_instructions("input.txt");
    let result1 = execute_instructions(&mut instructions.clone(), 7);
    println!("Part 1: {}", result1);

    let result2 = execute_instructions(&mut instructions, 12);
    println!("Part 2: {}", result2);
}
