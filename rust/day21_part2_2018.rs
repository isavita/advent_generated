
use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    println!("{}", solve(&input));
}

fn solve(input: &str) -> usize {
    let mut opcode_computer = parse_input(input);
    let mut last_reg5 = 0;
    let mut compared_register5s = HashSet::new();
    loop {
        if opcode_computer.registers[opcode_computer.instruction_pointer] == 28 {
            let reg5 = opcode_computer.registers[5];
            if compared_register5s.contains(&reg5) {
                break;
            }
            compared_register5s.insert(reg5);
            last_reg5 = reg5;
        }
        if opcode_computer.tick() {
            break;
        }
    }
    last_reg5
}

struct OpcodeComputer {
    instructions: Vec<Instruction>,
    registers: [usize; 6],
    instruction_pointer: usize,
}

struct Instruction {
    name: String,
    abc_values: [usize; 3],
}

impl OpcodeComputer {
    fn tick(&mut self) -> bool {
        let ip = self.registers[self.instruction_pointer];
        if ip >= self.instructions.len() {
            return true;
        }
        let inst = &self.instructions[ip];
        let opcode_func = opcode_names_to_funcs(&inst.name);
        self.registers = opcode_func(self.registers, inst.abc_values);
        self.registers[self.instruction_pointer] += 1;
        self.registers[self.instruction_pointer] >= self.instructions.len()
    }
}

fn parse_input(input: &str) -> OpcodeComputer {
    let lines: Vec<&str> = input.lines().collect();
    let instruction_pointer = lines[0]
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();
    let mut instructions = Vec::new();
    for l in &lines[1..] {
        let parts: Vec<&str> = l.split_whitespace().collect();
        let abc_values = [
            parts[1].parse().unwrap(),
            parts[2].parse().unwrap(),
            parts[3].parse().unwrap(),
        ];
        instructions.push(Instruction {
            name: parts[0].to_string(),
            abc_values,
        });
    }
    OpcodeComputer {
        instructions,
        registers: [0; 6],
        instruction_pointer,
    }
}

fn opcode_names_to_funcs(name: &str) -> fn([usize; 6], [usize; 3]) -> [usize; 6] {
    match name {
        "addr" => addr,
        "addi" => addi,
        "mulr" => mulr,
        "muli" => muli,
        "banr" => banr,
        "bani" => bani,
        "borr" => borr,
        "bori" => bori,
        "setr" => setr,
        "seti" => seti,
        "gtir" => gtir,
        "gtri" => gtri,
        "gtrr" => gtrr,
        "eqir" => eqir,
        "eqri" => eqri,
        "eqrr" => eqrr,
        _ => panic!("Unknown opcode name"),
    }
}

fn addr(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] + registers[abc_values[1]];
    registers
}

fn addi(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] + abc_values[1];
    registers
}

fn mulr(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] * registers[abc_values[1]];
    registers
}

fn muli(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] * abc_values[1];
    registers
}

fn banr(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] & registers[abc_values[1]];
    registers
}

fn bani(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] & abc_values[1];
    registers
}

fn borr(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] | registers[abc_values[1]];
    registers
}

fn bori(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]] | abc_values[1];
    registers
}

fn setr(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = registers[abc_values[0]];
    registers
}

fn seti(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = abc_values[0];
    registers
}

fn gtir(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = if abc_values[0] > registers[abc_values[1]] {
        1
    } else {
        0
    };
    registers
}

fn gtri(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = if registers[abc_values[0]] > abc_values[1] {
        1
    } else {
        0
    };
    registers
}

fn gtrr(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = if registers[abc_values[0]] > registers[abc_values[1]] {
        1
    } else {
        0
    };
    registers
}

fn eqir(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = if abc_values[0] == registers[abc_values[1]] {
        1
    } else {
        0
    };
    registers
}

fn eqri(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = if registers[abc_values[0]] == abc_values[1] {
        1
    } else {
        0
    };
    registers
}

fn eqrr(mut registers: [usize; 6], abc_values: [usize; 3]) -> [usize; 6] {
    registers[abc_values[2]] = if registers[abc_values[0]] == registers[abc_values[1]] {
        1
    } else {
        0
    };
    registers
}
