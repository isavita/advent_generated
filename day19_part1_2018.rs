
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut lines = input.lines();
    let ip_register: usize = lines.next().unwrap().split_whitespace().last().unwrap().parse().unwrap();
    let instructions: Vec<(&str, i32, i32, i32)> = lines.map(|line| {
        let parts: Vec<&str> = line.split_whitespace().collect();
        (parts[0], parts[1].parse().unwrap(), parts[2].parse().unwrap(), parts[3].parse().unwrap())
    }).collect();

    let mut registers = vec![0; 6];
    let mut ip = 0;

    while ip < instructions.len() {
        registers[ip_register] = ip as i32;
        match instructions[ip] {
            ("addr", a, b, c) => registers[c as usize] = registers[a as usize] + registers[b as usize],
            ("addi", a, b, c) => registers[c as usize] = registers[a as usize] + b,
            ("mulr", a, b, c) => registers[c as usize] = registers[a as usize] * registers[b as usize],
            ("muli", a, b, c) => registers[c as usize] = registers[a as usize] * b,
            ("banr", a, b, c) => registers[c as usize] = registers[a as usize] & registers[b as usize],
            ("bani", a, b, c) => registers[c as usize] = registers[a as usize] & b,
            ("borr", a, b, c) => registers[c as usize] = registers[a as usize] | registers[b as usize],
            ("bori", a, b, c) => registers[c as usize] = registers[a as usize] | b,
            ("setr", a, _, c) => registers[c as usize] = registers[a as usize],
            ("seti", a, _, c) => registers[c as usize] = a,
            ("gtir", a, b, c) => registers[c as usize] = if a > registers[b as usize] { 1 } else { 0 },
            ("gtri", a, b, c) => registers[c as usize] = if registers[a as usize] > b { 1 } else { 0 },
            ("gtrr", a, b, c) => registers[c as usize] = if registers[a as usize] > registers[b as usize] { 1 } else { 0 },
            ("eqir", a, b, c) => registers[c as usize] = if a == registers[b as usize] { 1 } else { 0 },
            ("eqri", a, b, c) => registers[c as usize] = if registers[a as usize] == b { 1 } else { 0 },
            ("eqrr", a, b, c) => registers[c as usize] = if registers[a as usize] == registers[b as usize] { 1 } else { 0 },
            _ => {}
        }
        ip = registers[ip_register] as usize + 1;
    }

    println!("{}", registers[0]);
}
