
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut program: Vec<i64> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();
    let mut computer = IntcodeComputer::new(program);
    let mut alignment_parameters = 0;

    computer.run();

    let mut scaffold = computer.get_output();
    let scaffold_str = scaffold.iter().map(|&x| x as u8 as char).collect::<String>();
    let scaffold_lines: Vec<&str> = scaffold_str.trim().split("\n").collect();

    for y in 1..scaffold_lines.len() - 1 {
        for x in 1..scaffold_lines[y].len() - 1 {
            if scaffold_lines[y].chars().nth(x).unwrap() == '#' &&
               scaffold_lines[y - 1].chars().nth(x).unwrap() == '#' &&
               scaffold_lines[y + 1].chars().nth(x).unwrap() == '#' &&
               scaffold_lines[y].chars().nth(x - 1).unwrap() == '#' &&
               scaffold_lines[y].chars().nth(x + 1).unwrap() == '#' {
                alignment_parameters += x * y;
            }
        }
    }

    println!("{}", alignment_parameters);
}

struct IntcodeComputer {
    memory: Vec<i64>,
    ip: usize,
    relative_base: i64,
    input: Vec<i64>,
    output: Vec<i64>,
}

impl IntcodeComputer {
    fn new(program: Vec<i64>) -> IntcodeComputer {
        let mut memory = vec![0; 10000];
        memory[..program.len()].copy_from_slice(&program);
        IntcodeComputer {
            memory,
            ip: 0,
            relative_base: 0,
            input: Vec::new(),
            output: Vec::new(),
        }
    }

    fn run(&mut self) {
        loop {
            let opcode = self.memory[self.ip] % 100;
            match opcode {
                1 | 2 | 7 | 8 => {
                    let p1 = self.get_value(self.ip + 1, (self.memory[self.ip] / 100) % 10);
                    let p2 = self.get_value(self.ip + 2, (self.memory[self.ip] / 1000) % 10);
                    let p3 = self.get_address(self.ip + 3, (self.memory[self.ip] / 10000) % 10);
                    match opcode {
                        1 => self.memory[p3] = p1 + p2,
                        2 => self.memory[p3] = p1 * p2,
                        7 => self.memory[p3] = (p1 < p2) as i64,
                        8 => self.memory[p3] = (p1 == p2) as i64,
                        _ => unreachable!(),
                    }
                    self.ip += 4;
                }
                3 => {
                    let p1 = self.get_address(self.ip + 1, (self.memory[self.ip] / 100) % 10);
                    self.memory[p1] = self.input.remove(0);
                    self.ip += 2;
                }
                4 => {
                    let p1 = self.get_value(self.ip + 1, (self.memory[self.ip] / 100) % 10);
                    self.output.push(p1);
                    self.ip += 2;
                }
                5 | 6 => {
                    let p1 = self.get_value(self.ip + 1, (self.memory[self.ip] / 100) % 10);
                    let p2 = self.get_value(self.ip + 2, (self.memory[self.ip] / 1000) % 10);
                    match opcode {
                        5 => {
                            if p1 != 0 {
                                self.ip = p2 as usize;
                            } else {
                                self.ip += 3;
                            }
                        }
                        6 => {
                            if p1 == 0 {
                                self.ip = p2 as usize;
                            } else {
                                self.ip += 3;
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                9 => {
                    let p1 = self.get_value(self.ip + 1, (self.memory[self.ip] / 100) % 10);
                    self.relative_base += p1;
                    self.ip += 2;
                }
                99 => break,
                _ => unreachable!(),
            }
        }
    }

    fn get_value(&self, pos: usize, mode: i64) -> i64 {
        match mode {
            0 => self.memory[self.memory[pos] as usize],
            1 => self.memory[pos],
            2 => self.memory[(self.relative_base + self.memory[pos]) as usize],
            _ => unreachable!(),
        }
    }

    fn get_address(&self, pos: usize, mode: i64) -> usize {
        match mode {
            0 => self.memory[pos] as usize,
            2 => (self.relative_base + self.memory[pos]) as usize,
            _ => unreachable!(),
        }
    }

    fn get_output(&self) -> Vec<i64> {
        self.output.clone()
    }
}
