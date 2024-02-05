
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let program: Vec<i64> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();

    let mut intcode = Intcode::new(program);
    intcode.run();

    let mut block_tiles = 0;
    for i in 0..intcode.output.len() / 3 {
        if intcode.output[i * 3 + 2] == 2 {
            block_tiles += 1;
        }
    }

    println!("{}", block_tiles);
}

struct Intcode {
    memory: Vec<i64>,
    ip: usize,
    relative_base: i64,
    input: Vec<i64>,
    output: Vec<i64>,
}

impl Intcode {
    fn new(program: Vec<i64>) -> Intcode {
        let mut memory = vec![0; 10000];
        memory[..program.len()].copy_from_slice(&program);

        Intcode {
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
                        7 => self.memory[p3] = if p1 < p2 { 1 } else { 0 },
                        8 => self.memory[p3] = if p1 == p2 { 1 } else { 0 },
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
                        5 => self.ip = if p1 != 0 { p2 as usize } else { self.ip + 3 },
                        6 => self.ip = if p1 == 0 { p2 as usize } else { self.ip + 3 },
                        _ => unreachable!(),
                    }
                }
                9 => {
                    let p1 = self.get_value(self.ip + 1, (self.memory[self.ip] / 100) % 10);
                    self.relative_base += p1;
                    self.ip += 2;
                }
                99 => break,
                _ => panic!("Invalid opcode"),
            }
        }
    }

    fn get_value(&mut self, pos: usize, mode: i64) -> i64 {
        match mode {
            0 => self.memory[self.memory[pos] as usize],
            1 => self.memory[pos],
            2 => self.memory[(self.relative_base + self.memory[pos]) as usize],
            _ => unreachable!(),
        }
    }

    fn get_address(&mut self, pos: usize, mode: i64) -> usize {
        match mode {
            0 => self.memory[pos] as usize,
            2 => (self.relative_base + self.memory[pos]) as usize,
            _ => unreachable!(),
        }
    }
}
