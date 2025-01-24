
use std::fs::read_to_string;
use std::collections::VecDeque;

#[derive(Clone)]
struct IntcodeComputer {
    memory: Vec<i64>,
    ip: usize,
    relative_base: i64,
    input: VecDeque<i64>,
    output: VecDeque<i64>,
    halted: bool,
}

impl IntcodeComputer {
    fn new(memory: Vec<i64>) -> Self {
        IntcodeComputer {
            memory,
            ip: 0,
            relative_base: 0,
            input: VecDeque::new(),
            output: VecDeque::new(),
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

    fn get_address(&mut self, mode: i64, offset: usize) -> usize {
        let value = self.read_memory(self.ip + offset);
        match mode {
            0 => value as usize,
            2 => (self.relative_base + value) as usize,
            _ => panic!("Invalid address mode"),
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
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let address = self.get_address(mode3, 3);
                    self.write_memory(address, param1 + param2);
                    self.ip += 4;
                }
                2 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let address = self.get_address(mode3, 3);
                    self.write_memory(address, param1 * param2);
                    self.ip += 4;
                }
                3 => {
                    let address = self.get_address(mode1, 1);
                    if let Some(value) = self.input.pop_front() {
                        self.write_memory(address, value);
                        self.ip += 2;
                    } else {
                        
                        return; 
                    }
                }
                4 => {
                    let param1 = self.get_parameter(mode1, 1);
                    self.output.push_back(param1);
                    self.ip += 2;
                }
                5 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    if param1 != 0 {
                        self.ip = param2 as usize;
                    } else {
                        self.ip += 3;
                    }
                }
                6 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    if param1 == 0 {
                        self.ip = param2 as usize;
                    } else {
                        self.ip += 3;
                    }
                }
                7 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let address = self.get_address(mode3, 3);
                    self.write_memory(address, if param1 < param2 { 1 } else { 0 });
                    self.ip += 4;
                }
                8 => {
                    let param1 = self.get_parameter(mode1, 1);
                    let param2 = self.get_parameter(mode2, 2);
                    let address = self.get_address(mode3, 3);
                    self.write_memory(address, if param1 == param2 { 1 } else { 0 });
                    self.ip += 4;
                }
                9 => {
                    let param1 = self.get_parameter(mode1, 1);
                    self.relative_base += param1;
                    self.ip += 2;
                }
                99 => {
                    self.halted = true;
                    
                }
                _ => panic!("Invalid opcode"),
            }
        }
    }
}

fn main() {
    let input = read_to_string("input.txt").expect("Failed to read input file");
    let memory: Vec<i64> = input.trim().split(',').map(|s| s.parse().unwrap()).collect();

    let mut computers: Vec<IntcodeComputer> = (0..50)
        .map(|i| {
            let mut computer = IntcodeComputer::new(memory.clone());
            computer.input.push_back(i);
            computer
        })
        .collect();

    let mut packet_queues: Vec<VecDeque<(i64, i64)>> = vec![VecDeque::new(); 50];

    loop {
        for i in 0..50 {
            
            if packet_queues[i].is_empty() {
              computers[i].input.push_back(-1);
            } else {
                let (x,y) = packet_queues[i].pop_front().unwrap();
                computers[i].input.push_back(x);
                computers[i].input.push_back(y);
            }
            
            computers[i].run();

            while computers[i].output.len() >= 3 {
                let address = computers[i].output.pop_front().unwrap();
                let x = computers[i].output.pop_front().unwrap();
                let y = computers[i].output.pop_front().unwrap();

                if address == 255 {
                    println!("{}", y);
                    return;
                }

                packet_queues[address as usize].push_back((x, y));
            }
        }
    }
}
