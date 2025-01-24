
use std::fs::File;
use std::io::{BufRead, BufReader};

fn solve(n: usize) -> usize {
    (1..=n).filter(|i| n % i == 0).sum()
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let lines: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();

    let ip_register: usize = lines[0]
        .split_whitespace()
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();

    let mut program = Vec::new();
    for line in &lines[1..] {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let a: usize = parts[1].parse().unwrap();
        let b: usize = parts[2].parse().unwrap();
        let c: usize = parts[3].parse().unwrap();
        program.push((parts[0], a, b, c));
    }

    let mut registers = [0; 6];
    registers[0] = 1;
    let mut ip = 0;
    for _ in 0..1000 {
        if ip >= program.len() {
            break;
        }
        let (op, a, b, c) = program[ip];
        registers[ip_register] = ip;
        match op {
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
            _ => unreachable!(),
        }
        ip = registers[ip_register] + 1;
    }

    let n = *registers.iter().max().unwrap();
    println!("{}", solve(n));
}
