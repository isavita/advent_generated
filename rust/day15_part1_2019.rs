
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::io;

#[derive(Debug)]
enum Status {
    Wall,
    Moved,
    OxygenSystem,
}

#[derive(Debug, Clone)]
struct IntcodeComputer {
    memory: Vec<i64>,
    pc: usize,
    relative_base: i64,
    input_queue: VecDeque<i64>,
    output_queue: VecDeque<i64>,
}

impl IntcodeComputer {
    fn new(program: &str) -> Self {
        let memory: Vec<i64> = program
            .trim()
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect();
        IntcodeComputer {
            memory,
            pc: 0,
            relative_base: 0,
            input_queue: VecDeque::new(),
            output_queue: VecDeque::new(),
        }
    }

    fn enqueue_input(&mut self, input: i64) {
        self.input_queue.push_back(input);
    }

    fn dequeue_output(&mut self) -> Option<i64> {
        self.output_queue.pop_front()
    }

    fn get_parameter(&mut self, offset: usize, mode: i64) -> i64 {
        let value = self.memory[self.pc + offset];
        match mode {
            0 => { // Position mode
                if value < 0 { panic!("Invalid memory address: {}", value); }
                if value as usize >= self.memory.len() { 0 } else { self.memory[value as usize] }
            },
            1 => value, // Immediate mode
            2 => { // Relative mode
                let address = self.relative_base + value;
                if address < 0 { panic!("Invalid memory address: {}", address); }
                if address as usize >= self.memory.len() { 0 } else { self.memory[address as usize] }
            }
            _ => panic!("Invalid parameter mode: {}", mode),
        }
    }

    fn get_parameter_address(&mut self, offset: usize, mode: i64) -> usize {
        let value = self.memory[self.pc + offset];
        match mode {
            0 => {
                if value < 0 { panic!("Invalid memory address: {}", value); }
                value as usize
            }, // Position mode
            2 => {
                let address = self.relative_base + value;
                if address < 0 { panic!("Invalid memory address: {}", address); }
                address as usize
            }, // Relative mode
            _ => panic!("Invalid parameter mode for address: {}", mode),
        }
    }


    fn run_instruction(&mut self) -> bool {
        let instruction = self.memory[self.pc];
        let opcode = instruction % 100;
        let mode1 = (instruction / 100) % 10;
        let mode2 = (instruction / 1000) % 10;
        let mode3 = (instruction / 10000) % 10;

        match opcode {
            1 => { // Add
                let param1 = self.get_parameter(1, mode1);
                let param2 = self.get_parameter(2, mode2);
                let address = self.get_parameter_address(3, mode3);
                self.ensure_memory_size(address);
                self.memory[address] = param1 + param2;
                self.pc += 4;
            }
            2 => { // Multiply
                let param1 = self.get_parameter(1, mode1);
                let param2 = self.get_parameter(2, mode2);
                let address = self.get_parameter_address(3, mode3);
                self.ensure_memory_size(address);
                self.memory[address] = param1 * param2;
                self.pc += 4;
            }
            3 => { // Input
                if self.input_queue.is_empty() {
                    return false; // Need more input, pause execution
                }
                let address = self.get_parameter_address(1, mode1);
                let input_value = self.input_queue.pop_front().unwrap();
                self.ensure_memory_size(address);
                self.memory[address] = input_value;
                self.pc += 2;
            }
            4 => { // Output
                let param1 = self.get_parameter(1, mode1);
                self.output_queue.push_back(param1);
                self.pc += 2;
            }
            5 => { // Jump-if-true
                let param1 = self.get_parameter(1, mode1);
                let param2 = self.get_parameter(2, mode2);
                if param1 != 0 {
                    self.pc = param2 as usize;
                } else {
                    self.pc += 3;
                }
            }
            6 => { // Jump-if-false
                let param1 = self.get_parameter(1, mode1);
                let param2 = self.get_parameter(2, mode2);
                if param1 == 0 {
                    self.pc = param2 as usize;
                } else {
                    self.pc += 3;
                }
            }
            7 => { // Less than
                let param1 = self.get_parameter(1, mode1);
                let param2 = self.get_parameter(2, mode2);
                let address = self.get_parameter_address(3, mode3);
                self.ensure_memory_size(address);
                self.memory[address] = if param1 < param2 { 1 } else { 0 };
                self.pc += 4;
            }
            8 => { // Equals
                let param1 = self.get_parameter(1, mode1);
                let param2 = self.get_parameter(2, mode2);
                let address = self.get_parameter_address(3, mode3);
                self.ensure_memory_size(address);
                self.memory[address] = if param1 == param2 { 1 } else { 0 };
                self.pc += 4;
            }
            9 => { // Adjust relative base
                let param1 = self.get_parameter(1, mode1);
                self.relative_base += param1;
                self.pc += 2;
            }
            99 => { // Halt
                return true;
            }
            _ => panic!("Invalid opcode: {}", opcode),
        }
        false // Continue execution
    }

    fn ensure_memory_size(&mut self, address: usize) {
        if address >= self.memory.len() {
            self.memory.resize(address + 1, 0);
        }
    }


    fn run_until_output(&mut self) -> Option<i64> {
        while self.output_queue.is_empty() {
            if self.run_instruction() {
                return None; // Program halted
            }
        }
        self.dequeue_output()
    }

    fn run_until_halt(&mut self) {
        while !self.run_instruction() {}
    }

    fn run_with_input(&mut self, input: i64) -> Option<i64> {
        self.enqueue_input(input);
        self.run_until_output()
    }
}


fn solve() -> io::Result<()> {
    let program_str = fs::read_to_string("input.txt")?;
    let initial_computer = IntcodeComputer::new(&program_str);

    let mut map: HashMap<(i32, i32), Status> = HashMap::new();
    let mut queue: VecDeque<((i32, i32), IntcodeComputer, i32)> = VecDeque::new();

    let start_pos = (0, 0);
    map.insert(start_pos, Status::Moved); // Starting position is open
    queue.push_back((start_pos, initial_computer, 0));

    let mut oxygen_system_distance = -1;

    let directions = [(1, -1, 0), (2, 1, 0), (3, 0, -1), (4, 0, 1)]; // N, S, W, E and their commands
    let reverse_directions = [2, 1, 4, 3]; // To move back

    while let Some(((x, y), mut computer, steps)) = queue.pop_front() {
        if oxygen_system_distance != -1 && steps >= oxygen_system_distance {
            continue; // Optimization: If we found oxygen and current path is longer, no need to explore further
        }

        for i in 0..4 {
            let dir = directions[i];
            let next_pos = (x + dir.1, y + dir.2);

            if !map.contains_key(&next_pos) {
                let mut next_computer = computer.clone();
                next_computer.enqueue_input(dir.0);
                let status_code_opt = next_computer.run_until_output();

                if let Some(status_code) = status_code_opt {
                    match status_code {
                        0 => {
                            map.insert(next_pos, Status::Wall);
                        }
                        1 => {
                            map.insert(next_pos, Status::Moved);
                            queue.push_back((next_pos, next_computer, steps + 1));
                        }
                        2 => {
                            map.insert(next_pos, Status::OxygenSystem);
                            if oxygen_system_distance == -1 || steps + 1 < oxygen_system_distance {
                                oxygen_system_distance = steps + 1;
                            }
                            queue.push_back((next_pos, next_computer, steps + 1)); // Continue exploring to find potentially shorter paths
                        }
                        _ => panic!("Unknown status code: {}", status_code),
                    }
                } else {
                    panic!("Program halted unexpectedly");
                }
            }
        }
    }

    println!("{}", oxygen_system_distance);

    Ok(())
}

fn main() -> io::Result<()> {
    solve()
}
