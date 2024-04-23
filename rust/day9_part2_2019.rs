use std::fs::File;
use std::io::{self, BufRead};
use std::collections::VecDeque;

struct IntcodeComputer {
    memory: Vec<i64>,
    ip: usize,
    relative_base: i64,
    input: VecDeque<i64>,
    output: Vec<i64>,
}

impl IntcodeComputer {
    fn new(memory: Vec<i64>) -> IntcodeComputer {
        IntcodeComputer {
            memory,
            ip: 0,
            relative_base: 0,
            input: VecDeque::new(),
            output: Vec::new(),
        }
    }

    fn run(&mut self) {
        loop {
            let opcode = self.memory[self.ip] % 100;
            let mode1 = (self.memory[self.ip] / 100) % 10;
            let mode2 = (self.memory[self.ip] / 1000) % 10;
            let mode3 = (self.memory[self.ip] / 10000) % 10;

            match opcode {
                1 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    let b = self.get_param(mode2, self.ip + 2);
                    let c = self.get_addr(mode3, self.ip + 3);
                    self.memory[c as usize] = a + b;
                    self.ip += 4;
                }
                2 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    let b = self.get_param(mode2, self.ip + 2);
                    let c = self.get_addr(mode3, self.ip + 3);
                    self.memory[c as usize] = a * b;
                    self.ip += 4;
                }
                3 => {
                    let a = self.get_addr(mode1, self.ip + 1);
                    self.memory[a as usize] = self.input.pop_front().unwrap_or(0);
                    self.ip += 2;
                }
                4 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    self.output.push(a);
                    self.ip += 2;
                }
                5 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    let b = self.get_param(mode2, self.ip + 2);
                    if a != 0 {
                        self.ip = b as usize;
                    } else {
                        self.ip += 3;
                    }
                }
                6 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    let b = self.get_param(mode2, self.ip + 2);
                    if a == 0 {
                        self.ip = b as usize;
                    } else {
                        self.ip += 3;
                    }
                }
                7 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    let b = self.get_param(mode2, self.ip + 2);
                    let c = self.get_addr(mode3, self.ip + 3);
                    if a < b {
                        self.memory[c as usize] = 1;
                    } else {
                        self.memory[c as usize] = 0;
                    }
                    self.ip += 4;
                }
                8 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    let b = self.get_param(mode2, self.ip + 2);
                    let c = self.get_addr(mode3, self.ip + 3);
                    if a == b {
                        self.memory[c as usize] = 1;
                    } else {
                        self.memory[c as usize] = 0;
                    }
                    self.ip += 4;
                }
                9 => {
                    let a = self.get_param(mode1, self.ip + 1);
                    self.relative_base += a;
                    self.ip += 2;
                }
                99 => {
                    break;
                }
                _ => {
                    panic!("Unknown opcode");
                }
            }
        }
    }

    fn get_param(&self, mode: i64, index: usize) -> i64 {
        match mode {
            0 => self.memory[self.memory[index] as usize],
            1 => self.memory[index],
            2 => self.memory[(self.relative_base + self.memory[index]) as usize],
            _ => panic!("Unknown mode"),
        }
    }

    fn get_addr(&self, mode: i64, index: usize) -> i64 {
        match mode {
            0 => self.memory[index] as i64,
            2 => self.relative_base + self.memory[index],
            _ => panic!("Unknown mode"),
        }
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut memory: Vec<i64> = reader
        .lines()
        .next()
        .unwrap()?
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect();

    memory.resize(10000, 0);

    let mut computer = IntcodeComputer::new(memory);
    computer.input.push_back(2); // Run in sensor boost mode
    computer.run();

    println!("Distress signal coordinates: {}", computer.output[0]);

    Ok(())
}