
use std::{
    fs::File,
    io::{self, BufRead},
    str::FromStr,
};

#[derive(Debug)]
struct Program {
    a: i64,
    b: i64,
    c: i64,
    program: Vec<i32>,
}

fn compute_operand(val: i64, a: i64, b: i64, c: i64) -> i64 {
    match val {
        0..=3 => val,
        4 => a,
        5 => b,
        6 => c,
        _ => panic!("Invalid combo operand: {}", val),
    }
}

fn simulate_computer(program: &Program, initial_a: i64) -> Vec<i32> {
    let mut outs = Vec::new();
    let mut a = initial_a;
    let mut b = program.b;
    let mut c = program.c;
    let input = &program.program;

    let mut i = 0;
    while i < input.len() {
        let cmd = input[i];
        match cmd {
            0 => {
                a >>= compute_operand(input[i + 1] as i64, a, b, c);
            }
            1 => {
                b ^= input[i + 1] as i64;
            }
            2 => {
                 b = compute_operand(input[i + 1] as i64, a, b, c) % 8;
            }
            3 => {
                if a != 0 {
                    i = (input[i + 1] - 1) as usize;
                    continue;
                }
            }
            4 => {
                b ^= c;
            }
            5 => {
                outs.push((compute_operand(input[i + 1] as i64, a, b, c) % 8) as i32);
            }
            6 => {
                b = a >> compute_operand(input[i + 1] as i64, a, b, c);
            }
            7 => {
                c = a >> compute_operand(input[i + 1] as i64, a, b, c);
            }
            _ => panic!("Invalid opcode: {}", cmd),
        }
        i += 2;
    }
    outs
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Pair {
    depth: usize,
    score: i64,
}

fn check(p: &Program) -> Vec<i64> {
    let program = &p.program;
    let mut valids = Vec::new();
    let mut stack = vec![Pair { depth: 0, score: 0 }];
    let mut seen = std::collections::HashSet::new();

    while let Some(state) = stack.pop() {
        if seen.contains(&state) {
            continue;
        }
        seen.insert(state);

        let depth = state.depth;
        let score = state.score;

        if depth == program.len() {
            valids.push(score);
        } else {
            for i in 0..8 {
                let new_score = i + 8 * score;
                let result = simulate_computer(p, new_score);
                if !result.is_empty() && result[0] == program[program.len() - 1 - depth] {
                    stack.push(Pair {
                        depth: depth + 1,
                        score: new_score,
                    });
                }
            }
        }
    }
    valids
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut a = 0;
    let mut b = 0;
    let mut c = 0;
    let mut program = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let line = line.trim();
        if line.starts_with("Register A:") {
            let parts: Vec<&str> = line.split(":").collect();
            a = i64::from_str(parts[1].trim()).unwrap();
        } else if line.starts_with("Register B:") {
            let parts: Vec<&str> = line.split(":").collect();
            b = i64::from_str(parts[1].trim()).unwrap();
        } else if line.starts_with("Register C:") {
            let parts: Vec<&str> = line.split(":").collect();
            c = i64::from_str(parts[1].trim()).unwrap();
        } else if line.starts_with("Program:") {
            let parts: Vec<&str> = line.split(":").collect();
            program = parts[1]
                .trim()
                .split(",")
                .map(|s| i32::from_str(s.trim()).unwrap())
                .collect();
        }
    }

    let p = Program { a, b, c, program };

    let valid_values = check(&p);
    let min_val = valid_values.iter().min().unwrap();

    println!("{}", min_val);

    Ok(())
}
