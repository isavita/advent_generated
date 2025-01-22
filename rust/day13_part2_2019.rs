
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("input.txt")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let program: Vec<i64> = contents
        .trim()
        .split(',')
        .map(|s| s.parse().expect("Failed to parse integer"))
        .collect();

    // Part 1
    let mut machine1 = IntcodeMachine::new(program.clone());
    let output1 = machine1.run();
    let count = output1
        .chunks(3)
        .filter(|chunk| chunk.len() == 3 && chunk[2] == 2)
        .count();
    println!("Part 1: {}", count);

    // Part 2
    let mut program2 = program.clone();
    program2[0] = 2;
    let mut machine2 = IntcodeMachine::new(program2);

    let mut score = 0;
    let mut ball_x = 0;
    let mut paddle_x = 0;

    while !machine2.halted {
        let output = machine2.run();
        
        for chunk in output.chunks(3) {
            if chunk.len() == 3 {
                let x = chunk[0];
                let y = chunk[1];
                let tile_type = chunk[2];
                 if x == -1 && y == 0 {
                    score = tile_type;
                } else if tile_type == 4 {
                    ball_x = x;
                } else if tile_type == 3 {
                    paddle_x = x;
                }
            }
        }
       
        let input = if ball_x < paddle_x {
            -1
        } else if ball_x > paddle_x {
            1
        } else {
            0
        };

        machine2.input.push(input);

    }


    println!("Part 2: {}", score);
    Ok(())
}

struct IntcodeMachine {
    memory: Vec<i64>,
    pc: usize,
    relative_base: i64,
    input: Vec<i64>,
    output: Vec<i64>,
    halted: bool,
}

impl IntcodeMachine {
    fn new(program: Vec<i64>) -> Self {
        IntcodeMachine {
            memory: program,
            pc: 0,
            relative_base: 0,
            input: Vec::new(),
            output: Vec::new(),
            halted: false,
        }
    }

    fn get_param_value(&mut self, mode: i64, param_index: usize) -> i64 {
        let value = self.memory[self.pc + param_index] as usize;
        match mode {
            0 => self.memory.get(value).copied().unwrap_or(0),
            1 => self.memory[self.pc + param_index],
            2 => {
                let addr = (self.relative_base + value as i64) as usize;
                self.memory.get(addr).copied().unwrap_or(0)
            }
            _ => panic!("Invalid parameter mode: {}", mode),
        }
    }
    fn get_param_address(&mut self, mode: i64, param_index: usize) -> usize {
        let value = self.memory[self.pc + param_index] as i64;
        match mode {
            0 => value as usize,
            2 => (self.relative_base + value) as usize,
            _ => panic!("Invalid address mode: {}", mode),
        }
    }

    fn resize_memory_if_needed(&mut self, addr: usize) {
        if addr >= self.memory.len() {
            self.memory.resize(addr + 1, 0);
        }
    }

    fn run(&mut self) -> Vec<i64> {
       self.output.clear();

        while self.pc < self.memory.len() {
            let instruction = self.memory[self.pc];
            let opcode = instruction % 100;
            let mode1 = (instruction / 100) % 10;
            let mode2 = (instruction / 1000) % 10;
            let mode3 = (instruction / 10000) % 10;

            match opcode {
                1 => {
                    let param1 = self.get_param_value(mode1, 1);
                    let param2 = self.get_param_value(mode2, 2);
                    let addr = self.get_param_address(mode3, 3);
                    self.resize_memory_if_needed(addr);
                    self.memory[addr] = param1 + param2;
                    self.pc += 4;
                }
                2 => {
                    let param1 = self.get_param_value(mode1, 1);
                    let param2 = self.get_param_value(mode2, 2);
                     let addr = self.get_param_address(mode3, 3);
                    self.resize_memory_if_needed(addr);
                    self.memory[addr] = param1 * param2;
                    self.pc += 4;
                }
                3 => {
                   let addr = self.get_param_address(mode1, 1);
                     self.resize_memory_if_needed(addr);
                    if let Some(input) = self.input.pop() {
                         self.memory[addr] = input;
                    } else {
                        
                          return self.output.clone();
                    }
                    self.pc += 2;
                }
                4 => {
                    let param1 = self.get_param_value(mode1, 1);
                    self.output.push(param1);
                    self.pc += 2;
                }
                5 => {
                    let param1 = self.get_param_value(mode1, 1);
                    let param2 = self.get_param_value(mode2, 2);
                    if param1 != 0 {
                        self.pc = param2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                6 => {
                    let param1 = self.get_param_value(mode1, 1);
                    let param2 = self.get_param_value(mode2, 2);
                    if param1 == 0 {
                        self.pc = param2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                7 => {
                    let param1 = self.get_param_value(mode1, 1);
                    let param2 = self.get_param_value(mode2, 2);
                    let addr = self.get_param_address(mode3, 3);
                    self.resize_memory_if_needed(addr);
                    self.memory[addr] = if param1 < param2 { 1 } else { 0 };
                    self.pc += 4;
                }
                8 => {
                    let param1 = self.get_param_value(mode1, 1);
                    let param2 = self.get_param_value(mode2, 2);
                    let addr = self.get_param_address(mode3, 3);
                    self.resize_memory_if_needed(addr);
                    self.memory[addr] = if param1 == param2 { 1 } else { 0 };
                    self.pc += 4;
                }
                9 => {
                    let param1 = self.get_param_value(mode1, 1);
                    self.relative_base += param1;
                    self.pc += 2;
                }
                99 => {
                    self.halted = true;
                     return self.output.clone();
                }
                _ => panic!("Invalid opcode: {}", opcode),
            }
        }
        return self.output.clone();
    }
}
