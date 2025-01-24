
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Clone, Copy, Debug)]
enum Opcode {
    Addr,
    Addi,
    Mulr,
    Muli,
    Banr,
    Bani,
    Borr,
    Bori,
    Setr,
    Seti,
    Gtir,
    Gtri,
    Gtrr,
    Eqir,
    Eqri,
    Eqrr,
}

fn execute(opcode: Opcode, a: usize, b: usize, c: usize, registers: &mut [usize; 4]) {
    registers[c] = match opcode {
        Opcode::Addr => registers[a] + registers[b],
        Opcode::Addi => registers[a] + b,
        Opcode::Mulr => registers[a] * registers[b],
        Opcode::Muli => registers[a] * b,
        Opcode::Banr => registers[a] & registers[b],
        Opcode::Bani => registers[a] & b,
        Opcode::Borr => registers[a] | registers[b],
        Opcode::Bori => registers[a] | b,
        Opcode::Setr => registers[a],
        Opcode::Seti => a,
        Opcode::Gtir => (a > registers[b]) as usize,
        Opcode::Gtri => (registers[a] > b) as usize,
        Opcode::Gtrr => (registers[a] > registers[b]) as usize,
        Opcode::Eqir => (a == registers[b]) as usize,
        Opcode::Eqri => (registers[a] == b) as usize,
        Opcode::Eqrr => (registers[a] == registers[b]) as usize,
    };
}

fn main() -> std::io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();

    let mut samples = Vec::new();
    let mut program = Vec::new();
    let mut parsing_samples = true;

    for i in 0..lines.len() {
        if lines[i].starts_with("Before") {
            let before: Vec<usize> = lines[i][9..19]
                .split(", ")
                .map(|s| s.parse().unwrap())
                .collect();
            let instruction: Vec<usize> = lines[i + 1]
                .split_whitespace()
                .map(|s| s.parse().unwrap())
                .collect();
            let after: Vec<usize> = lines[i + 2][9..19]
                .split(", ")
                .map(|s| s.parse().unwrap())
                .collect();
            samples.push((before, instruction, after));
        } else if parsing_samples && lines[i].is_empty() && i + 1 < lines.len() && lines[i+1].is_empty() {
            parsing_samples = false;
        } else if !parsing_samples && !lines[i].is_empty() {
            program.push(
                lines[i]
                    .split_whitespace()
                    .map(|s| s.parse().unwrap())
                    .collect::<Vec<usize>>(),
            );
        }
    }

    let mut count = 0;
    let mut possible_opcodes = vec![vec![true; 16]; 16];

    for (before, instruction, after) in &samples {
        let mut matching_opcodes = 0;
        for (op_index, opcode) in [
            Opcode::Addr,
            Opcode::Addi,
            Opcode::Mulr,
            Opcode::Muli,
            Opcode::Banr,
            Opcode::Bani,
            Opcode::Borr,
            Opcode::Bori,
            Opcode::Setr,
            Opcode::Seti,
            Opcode::Gtir,
            Opcode::Gtri,
            Opcode::Gtrr,
            Opcode::Eqir,
            Opcode::Eqri,
            Opcode::Eqrr,
        ]
        .iter()
        .enumerate()
        {
            let mut registers = [before[0], before[1], before[2], before[3]];
            execute(*opcode, instruction[1], instruction[2], instruction[3], &mut registers);
            if registers == [after[0], after[1], after[2], after[3]] {
                matching_opcodes += 1;
            } else {
                possible_opcodes[instruction[0]][op_index] = false;
            }
        }
        if matching_opcodes >= 3 {
            count += 1;
        }
    }

    println!("{}", count);

    let mut opcode_map = [0; 16];
    let mut found = vec![false; 16];
    while found.iter().any(|&x| !x) {
        for i in 0..16 {
            if !found[i] {
                let possible: Vec<usize> = possible_opcodes[i]
                    .iter()
                    .enumerate()
                    .filter(|&(_, &b)| b)
                    .map(|(index, _)| index)
                    .collect();
                if possible.len() == 1 {
                    opcode_map[i] = possible[0];
                    found[i] = true;
                    for j in 0..16 {
                        if i != j {
                            possible_opcodes[j][possible[0]] = false;
                        }
                    }
                }
            }
        }
    }

    let mut registers = [0; 4];
    for instruction in &program {
        let opcode = match opcode_map[instruction[0]] {
            0 => Opcode::Addr,
            1 => Opcode::Addi,
            2 => Opcode::Mulr,
            3 => Opcode::Muli,
            4 => Opcode::Banr,
            5 => Opcode::Bani,
            6 => Opcode::Borr,
            7 => Opcode::Bori,
            8 => Opcode::Setr,
            9 => Opcode::Seti,
            10 => Opcode::Gtir,
            11 => Opcode::Gtri,
            12 => Opcode::Gtrr,
            13 => Opcode::Eqir,
            14 => Opcode::Eqri,
            15 => Opcode::Eqrr,
            _ => unreachable!(),
        };
        execute(opcode, instruction[1], instruction[2], instruction[3], &mut registers);
    }

    println!("{}", registers[0]);

    Ok(())
}
