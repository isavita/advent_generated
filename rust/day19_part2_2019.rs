
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let program: Vec<i64> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();

    let mut affected_points = 0;
    for y in 0..50 {
        for x in 0..50 {
            let mut computer = IntcodeComputer::new(program.clone());
            computer.add_input(x);
            computer.add_input(y);
            computer.run();
            affected_points += computer.get_output()[0];
        }
    }
    println!("{}", affected_points);

    let mut x = 0;
    let mut y = 99;
    loop {
        while !is_affected(&program, x, y) {
            x += 1;
        }
        if is_affected(&program, x + 99, y - 99) {
            println!("{}", x * 10000 + y - 99);
            break;
        }
        y += 1;
    }
}

fn is_affected(program: &Vec<i64>, x: i64, y: i64) -> bool {
    let mut computer = IntcodeComputer::new(program.clone());
    computer.add_input(x);
    computer.add_input(y);
    computer.run();
    computer.get_output()[0] == 1
}

struct IntcodeComputer {
    memory: Vec<i64>,
    ip: usize,
    relative_base: i64,
    inputs: Vec<i64>,
    outputs: Vec<i64>,
}

impl IntcodeComputer {
    fn new(program: Vec<i64>) -> IntcodeComputer {
        let mut memory = vec![0; 100000];
        for (i, value) in program.iter().enumerate() {
            memory[i] = *value;
        }
        IntcodeComputer {
            memory,
            ip: 0,
            relative_base: 0,
            inputs: Vec::new(),
            outputs: Vec::new(),
        }
    }

    fn add_input(&mut self, input: i64) {
        self.inputs.push(input);
    }

    fn get_output(&self) -> Vec<i64> {
        self.outputs.clone()
    }

    fn run(&mut self) {
        loop {
            let opcode = self.memory[self.ip] % 100;
            match opcode {
                1 | 2 | 7 | 8 => {
                    let p1 = self.get_value(self.ip + 1, self.get_mode(0));
                    let p2 = self.get_value(self.ip + 2, self.get_mode(1));
                    let dest = self.get_address(self.ip + 3, self.get_mode(2));
                    match opcode {
                        1 => self.memory[dest] = p1 + p2,
                        2 => self.memory[dest] = p1 * p2,
                        7 => self.memory[dest] = if p1 < p2 { 1 } else { 0 },
                        8 => self.memory[dest] = if p1 == p2 { 1 } else { 0 },
                        _ => (),
                    }
                    self.ip += 4;
                }
                3 => {
                    let dest = self.get_address(self.ip + 1, self.get_mode(0));
                    self.memory[dest] = self.inputs.remove(0);
                    self.ip += 2;
                }
                4 => {
                    let output = self.get_value(self.ip + 1, self.get_mode(0));
                    self.outputs.push(output);
                    self.ip += 2;
                }
                5 | 6 => {
                    let p1 = self.get_value(self.ip + 1, self.get_mode(0));
                    let p2 = self.get_value(self.ip + 2, self.get_mode(1));
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
                        _ => (),
                    }
                }
                9 => {
                    self.relative_base += self.get_value(self.ip + 1, self.get_mode(0));
                    self.ip += 2;
                }
                99 => break,
                _ => (),
            }
        }
    }

    fn get_value(&mut self, address: usize, mode: i64) -> i64 {
        match mode {
            0 => self.memory[self.memory[address] as usize],
            1 => self.memory[address],
            2 => self.memory[(self.relative_base + self.memory[address]) as usize],
            _ => 0,
        }
    }

    fn get_address(&mut self, address: usize, mode: i64) -> usize {
        match mode {
            0 => self.memory[address] as usize,
            2 => (self.relative_base + self.memory[address]) as usize,
            _ => 0,
        }
    }

    fn get_mode(&self, param_num: usize) -> i64 {
        let opcode = self.memory[self.ip];
        let mode = opcode / 100;
        (mode / 10_i64.pow(param_num as u32)) % 10
    }
}
