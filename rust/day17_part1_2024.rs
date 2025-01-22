
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

#[derive(Debug)]
struct State {
    a: i32,
    b: i32,
    c: i32,
    program: Vec<i32>,
    ip: usize,
    output: Vec<i32>,
}

impl State {
    fn new(a: i32, b: i32, c: i32, program: Vec<i32>) -> Self {
        State {
            a,
            b,
            c,
            program,
            ip: 0,
            output: Vec::new(),
        }
    }

    fn get_combo_operand_value(&self, operand: i32) -> i32 {
        match operand {
            0..=3 => operand,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            _ => panic!("Invalid combo operand: {}", operand),
        }
    }

    fn run(&mut self) {
        while self.ip < self.program.len() {
            let opcode = self.program[self.ip];
            if self.ip + 1 >= self.program.len() {
                 break; // Halt if we try to read operand past the end
            }
            let operand = self.program[self.ip + 1];

            match opcode {
                0 => {
                    // adv
                    let divisor = 2_i32.pow(self.get_combo_operand_value(operand) as u32);
                    self.a = self.a / divisor;
                    self.ip += 2;
                }
                1 => {
                    // bxl
                    self.b = self.b ^ operand;
                    self.ip += 2;
                }
                2 => {
                    // bst
                    self.b = self.get_combo_operand_value(operand) % 8;
                    self.ip += 2;
                }
                3 => {
                    // jnz
                    if self.a != 0 {
                        self.ip = operand as usize;
                    } else {
                        self.ip += 2;
                    }
                }
                4 => {
                    // bxc
                    self.b = self.b ^ self.c;
                    self.ip += 2;
                }
                5 => {
                    // out
                    self.output.push(self.get_combo_operand_value(operand) % 8);
                    self.ip += 2;
                }
                6 => {
                     // bdv
                    let divisor = 2_i32.pow(self.get_combo_operand_value(operand) as u32);
                    self.b = self.a / divisor;
                     self.ip += 2;
                }
                7 => {
                     // cdv
                    let divisor = 2_i32.pow(self.get_combo_operand_value(operand) as u32);
                    self.c = self.a / divisor;
                    self.ip += 2;
                }
                _ => panic!("Invalid opcode: {}", opcode),
            }
        }
    }
}

fn parse_input(lines: &Vec<String>) -> (i32, i32, i32, Vec<i32>) {
    let mut a = 0;
    let mut b = 0;
    let mut c = 0;
    let mut program = Vec::new();

    for line in lines {
        if line.starts_with("Register A: ") {
            a = line["Register A: ".len()..].trim().parse().unwrap();
        } else if line.starts_with("Register B: ") {
            b = line["Register B: ".len()..].trim().parse().unwrap();
        } else if line.starts_with("Register C: ") {
            c = line["Register C: ".len()..].trim().parse().unwrap();
        } else if line.starts_with("Program: ") {
            program = line["Program: ".len()..]
                .trim()
                .split(',')
                .map(|s| i32::from_str(s.trim()).unwrap())
                .collect();
        }
    }

    (a, b, c, program)
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let (a, b, c, program) = parse_input(&lines);

    let mut state = State::new(a, b, c, program);
    state.run();

    let output_string = state
        .output
        .iter()
        .map(|&x| x.to_string())
        .collect::<Vec<String>>()
        .join(",");

    println!("{}", output_string);
    Ok(())
}
