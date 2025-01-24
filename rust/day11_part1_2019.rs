
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug, Clone)]
struct IntcodeComputer {
    memory: Vec<i64>,
    ip: usize,
    relative_base: i64,
    input: Vec<i64>,
    output: Vec<i64>,
    halted: bool,
}

impl IntcodeComputer {
    fn new(program: Vec<i64>) -> Self {
        IntcodeComputer {
            memory: program,
            ip: 0,
            relative_base: 0,
            input: Vec::new(),
            output: Vec::new(),
            halted: false,
        }
    }

    fn read_memory(&mut self, address: usize) -> i64 {
        if address >= self.memory.len() {
            self.memory.resize(address + 1, 0);
        }
        self.memory[address]
    }

    fn write_memory(&mut self, address: usize, value: i64) {
        if address >= self.memory.len() {
            self.memory.resize(address + 1, 0);
        }
        self.memory[address] = value;
    }

    fn get_parameter(&mut self, mode: i64, offset: usize) -> i64 {
        let value = self.read_memory(self.ip + offset);
        match mode {
            0 => self.read_memory(value as usize),
            1 => value,
            2 => self.read_memory((self.relative_base + value) as usize),
            _ => panic!("Invalid parameter mode"),
        }
    }

    fn get_write_address(&mut self, mode: i64, offset: usize) -> usize {
        let value = self.read_memory(self.ip + offset);
        match mode {
            0 => value as usize,
            2 => (self.relative_base + value) as usize,
            _ => panic!("Invalid parameter mode for write address"),
        }
    }

    fn run(&mut self) {
        while !self.halted {
            let instruction = self.read_memory(self.ip);
            let opcode = instruction % 100;
            let mode1 = (instruction / 100) % 10;
            let mode2 = (instruction / 1000) % 10;
            let mode3 = (instruction / 10000) % 10;

            match opcode {
                1 => {
                    // Add
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let result_address = self.get_write_address(mode3, 3);
                    self.write_memory(result_address, param1 + param2);
                    self.ip += 4;
                }
                2 => {
                    // Multiply
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let result_address = self.get_write_address(mode3, 3);
                    self.write_memory(result_address, param1 * param2);
                    self.ip += 4;
                }
                3 => {
                    // Input
                    if self.input.is_empty() {
                        return; // Wait for more input
                    }
                    let input_value = self.input.remove(0);
                    let result_address = self.get_write_address(mode1, 1);
                    self.write_memory(result_address, input_value);
                    self.ip += 2;
                }
                4 => {
                    // Output
                    let param1 = self.get_parameter(mode1, 1);
                    self.output.push(param1);
                    self.ip += 2;
                }
                5 => {
                    // Jump-if-true
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    if param1 != 0 {
                        self.ip = param2 as usize;
                    } else {
                        self.ip += 3;
                    }
                }
                6 => {
                    // Jump-if-false
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    if param1 == 0 {
                        self.ip = param2 as usize;
                    } else {
                        self.ip += 3;
                    }
                }
                7 => {
                    // Less than
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let result_address = self.get_write_address(mode3, 3);
                    self.write_memory(result_address, if param1 < param2 { 1 } else { 0 });
                    self.ip += 4;
                }
                8 => {
                    // Equals
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let result_address = self.get_write_address(mode3, 3);
                    self.write_memory(result_address, if param1 == param2 { 1 } else { 0 });
                    self.ip += 4;
                }
                9 => {
                    // Adjust relative base
                    let param1 = self.get_parameter(mode1, 1);
                    self.relative_base += param1;
                    self.ip += 2;
                }
                99 => {
                    // Halt
                    self.halted = true;
                }
                _ => panic!("Unknown opcode: {}", opcode),
            }
        }
    }
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn main() -> io::Result<()> {
    let lines = read_lines("input.txt")?;
    let program_str = lines.flatten().next().unwrap();
    let program: Vec<i64> = program_str.split(',').map(|s| s.parse().unwrap()).collect();

    let mut computer = IntcodeComputer::new(program);
    let mut grid: HashMap<(i32, i32), i64> = HashMap::new();
    let mut x = 0;
    let mut y = 0;
    let mut direction = 0; // 0: up, 1: right, 2: down, 3: left
    let directions = [(0, -1), (1, 0), (0, 1), (-1, 0)];

    loop {
        let current_color = *grid.get(&(x, y)).unwrap_or(&0);
        computer.input.push(current_color);
        computer.run();

        if computer.halted && computer.output.len() < 2 {
            break;
        }
        
        if computer.output.len() >= 2 {
            let color = computer.output.remove(0);
            let turn = computer.output.remove(0);

            grid.insert((x, y), color);

            direction = (direction + if turn == 0 { 3 } else { 1 }) % 4;
            x += directions[direction].0;
            y += directions[direction].1;
        } else if computer.halted {
            break;
        }
    }

    println!("{}", grid.len());

    Ok(())
}
