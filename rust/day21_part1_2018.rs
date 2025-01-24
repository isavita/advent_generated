
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    println!("{}", solve(&input));
}

fn solve(input: &str) -> usize {
    let mut lines = input.lines();
    let instruction_pointer: usize = lines.next().unwrap()[4..].trim().parse().unwrap();
    let instructions: Vec<_> = lines
        .map(|line| {
            let parts: Vec<_> = line.split_whitespace().collect();
            let name = parts[0];
            let a: usize = parts[1].parse().unwrap();
            let b: usize = parts[2].parse().unwrap();
            let c: usize = parts[3].parse().unwrap();
            (name, a, b, c)
        })
        .collect();

    let mut registers = [0; 6];

    loop {
        let instruction_index = registers[instruction_pointer];
        if instruction_index >= instructions.len() {
            break;
        }

        let (name, a, b, c) = instructions[instruction_index];

        match name {
            "addr" => registers[c] = registers[a] + registers[b],
            "addi" => registers[c] = registers[a] + b,
            "mulr" => registers[c] = registers[a] * registers[b],
            "muli" => registers[c] = registers[a] * b,
            "banr" => registers[c] = registers[a] & registers[b],
            "bani" => registers[c] = registers[a] & b,
            "borr" => registers[c] = registers[a] | registers[b],
            "bori" => registers[c] = registers[a] | b,
            "setr" => registers[c] = registers[a],
            "seti" => registers[c] = a,
            "gtir" => registers[c] = if a > registers[b] { 1 } else { 0 },
            "gtri" => registers[c] = if registers[a] > b { 1 } else { 0 },
            "gtrr" => registers[c] = if registers[a] > registers[b] { 1 } else { 0 },
            "eqir" => registers[c] = if a == registers[b] { 1 } else { 0 },
            "eqri" => registers[c] = if registers[a] == b { 1 } else { 0 },
            "eqrr" => registers[c] = if registers[a] == registers[b] { 1 } else { 0 },
            _ => panic!("Unknown opcode"),
        }

        registers[instruction_pointer] += 1;

        if registers[instruction_pointer] == 28 {
            break;
        }
    }

    registers[5]
}
