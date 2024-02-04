use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let program: Vec<i64> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();

    let mut count = 0;
    for y in 0..50 {
        for x in 0..50 {
            let mut computer = IntcodeComputer::new(program.clone());
            computer.add_input(x);
            computer.add_input(y);
            computer.run();
            count += computer.get_output()[0];
        }
    }

    println!("{}", count);
}

struct IntcodeComputer {
    memory: Vec<i64>,
    input: Vec<i64>,
    output: Vec<i64>,
    ip: usize,
    relative_base: i64,
}

impl IntcodeComputer {
    fn new(program: Vec<i64>) -> IntcodeComputer {
        let mut memory = vec![0; 10000];
        for (i, v) in program.iter().enumerate() {
            memory[i] = *v;
        }

        IntcodeComputer {
            memory,
            input: Vec::new(),
            output: Vec::new(),
            ip: 0,
            relative_base: 0,
        }
    }

    fn add_input(&mut self, value: i64) {
        self.input.push(value);
    }

    fn get_output(&self) -> Vec<i64> {
        self.output.clone()
    }

    fn run(&mut self) {
        loop {
            let opcode = self.memory[self.ip] % 100;
            match opcode {
                1 | 2 | 7 | 8 => {
                    let p1 = self.get_value(self.ip + 1, self.get_mode(0));
                    let p2 = self.get_value(self.ip + 2, self.get_mode(1));
                    let dest = self.get_address(self.ip + 3, self.get_mode(2));
                    self.ip += 4;
                    match opcode {
                        1 => self.memory[dest] = p1 + p2,
                        2 => self.memory[dest] = p1 * p2,
                        7 => self.memory[dest] = if p1 < p2 { 1 } else { 0 },
                        8 => self.memory[dest] = if p1 == p2 { 1 } else { 0 },
                        _ => unreachable!(),
                    }
                }
                3 => {
                    let dest = self.get_address(self.ip + 1, self.get_mode(0));
                    self.memory[dest] = self.input.remove(0);
                    self.ip += 2;
                }
                4 => {
                    let value = self.get_value(self.ip + 1, self.get_mode(0));
                    self.output.push(value);
                    self.ip += 2;
                }
                5 | 6 => {
                    let p1 = self.get_value(self.ip + 1, self.get_mode(0));
                    let p2 = self.get_value(self.ip + 2, self.get_mode(1));
                    self.ip = if opcode == 5 && p1 != 0 || opcode == 6 && p1 == 0 {
                        p2 as usize
                    } else {
                        self.ip + 3
                    };
                }
                9 => {
                    let value = self.get_value(self.ip + 1, self.get_mode(0));
                    self.relative_base += value;
                    self.ip += 2;
                }
                99 => break,
                _ => unreachable!(),
            }
        }
    }

    fn get_value(&mut self, index: usize, mode: i64) -> i64 {
        match mode {
            0 => self.memory[self.memory[index] as usize],
            1 => self.memory[index],
            2 => self.memory[(self.relative_base + self.memory[index]) as usize],
            _ => unreachable!(),
        }
    }

    fn get_address(&mut self, index: usize, mode: i64) -> usize {
        match mode {
            0 => self.memory[index] as usize,
            2 => (self.relative_base + self.memory[index]) as usize,
            _ => unreachable!(),
        }
    }

    fn get_mode(&self, offset: usize) -> i64 {
        (self.memory[self.ip] / 10i64.pow(offset as u32 + 2)) % 10
    }
}