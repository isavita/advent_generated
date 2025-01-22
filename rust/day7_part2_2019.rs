
use std::fs;
use std::io;
use std::collections::VecDeque;
use std::iter;

#[derive(Debug, Clone)]
struct IntcodeComputer {
    memory: Vec<i32>,
    pc: usize,
    inputs: VecDeque<i32>,
    outputs: Vec<i32>,
    halted: bool,
}

impl IntcodeComputer {
    fn new(memory: Vec<i32>) -> Self {
        IntcodeComputer {
            memory,
            pc: 0,
            inputs: VecDeque::new(),
            outputs: Vec::new(),
            halted: false,
        }
    }

    fn add_input(&mut self, input: i32) {
        self.inputs.push_back(input);
    }

    fn get_output(&mut self) -> Option<i32> {
        if self.outputs.is_empty() {
            None
        }
        else {
            Some(self.outputs.remove(0))
        }
    }

    fn run(&mut self) {
        loop {
            let opcode = self.memory[self.pc] % 100;
            let mode1 = (self.memory[self.pc] / 100) % 10;
            let mode2 = (self.memory[self.pc] / 1000) % 10;
            
            match opcode {
                1 => {
                    let param1 = self.get_parameter(1, mode1);
                    let param2 = self.get_parameter(2, mode2);
                    let param3 = self.memory[self.pc + 3] as usize;
                    self.memory[param3] = param1 + param2;
                    self.pc += 4;
                }
                2 => {
                   let param1 = self.get_parameter(1, mode1);
                    let param2 = self.get_parameter(2, mode2);
                    let param3 = self.memory[self.pc + 3] as usize;
                    self.memory[param3] = param1 * param2;
                    self.pc += 4;
                }
                3 => {
                    if let Some(input) = self.inputs.pop_front() {
                        let param1 = self.memory[self.pc + 1] as usize;
                        self.memory[param1] = input;
                        self.pc += 2;
                    } else {
                        return;
                    }
                }
                4 => {
                    let param1 = self.get_parameter(1, mode1);
                   self.outputs.push(param1);
                    self.pc += 2;
                }
                5 => {
                   let param1 = self.get_parameter(1, mode1);
                    let param2 = self.get_parameter(2, mode2);
                    if param1 != 0 {
                        self.pc = param2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                6 => {
                   let param1 = self.get_parameter(1, mode1);
                   let param2 = self.get_parameter(2, mode2);
                    if param1 == 0 {
                        self.pc = param2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                7 => {
                   let param1 = self.get_parameter(1, mode1);
                   let param2 = self.get_parameter(2, mode2);
                   let param3 = self.memory[self.pc + 3] as usize;
                    self.memory[param3] = if param1 < param2 { 1 } else { 0 };
                    self.pc += 4;
                }
                8 => {
                    let param1 = self.get_parameter(1, mode1);
                    let param2 = self.get_parameter(2, mode2);
                    let param3 = self.memory[self.pc + 3] as usize;
                    self.memory[param3] = if param1 == param2 { 1 } else { 0 };
                    self.pc += 4;
                }
                99 => {
                    self.halted = true;
                    return;
                }
                _ => panic!("Invalid opcode: {}", opcode),
            }
        }
    }

      fn get_parameter(&self, offset: usize, mode: i32) -> i32 {
        let value = self.memory[self.pc + offset];
        match mode {
            0 => self.memory[value as usize],
            1 => value,
            _ => panic!("Invalid parameter mode: {}", mode),
        }
    }

    fn is_halted(&self) -> bool{
        self.halted
    }
}


fn solve_part1(memory: &Vec<i32>) -> i32 {
    let mut max_output = 0;
    for phase_settings in permute(&[0, 1, 2, 3, 4]) {
        let mut output = 0;
        for phase in phase_settings {
           let mut computer = IntcodeComputer::new(memory.clone());
            computer.add_input(phase);
            computer.add_input(output);
            computer.run();
            output = computer.get_output().unwrap();
        }
         max_output = max_output.max(output);
    }

    max_output
}

fn solve_part2(memory: &Vec<i32>) -> i32 {
     let mut max_output = 0;
    for phase_settings in permute(&[5, 6, 7, 8, 9]) {
        let mut computers: Vec<IntcodeComputer> = phase_settings
            .iter()
            .map(|&phase| {
                let mut computer = IntcodeComputer::new(memory.clone());
                computer.add_input(phase);
                computer
            })
            .collect();

        let mut output = 0;
        let mut i = 0;
        let mut all_halted = false;
        while !all_halted {
           computers[i].add_input(output);
           computers[i].run();
           if let Some(new_output) = computers[i].get_output() {
            output = new_output;
           } 
          
            i = (i + 1) % 5;
            all_halted = computers.iter().all(|computer| computer.is_halted());
        }
         max_output = max_output.max(output);
    }

    max_output
}

fn permute<T: Copy>(arr: &[T]) -> Vec<Vec<T>> {
    if arr.len() == 0 {
       return vec![vec![]];
    }

    let mut permutations = Vec::new();
    for (i, &val) in arr.iter().enumerate() {
        let mut remaining = arr.to_vec();
        remaining.remove(i);
        for p in permute(&remaining) {
            let mut v = vec![val];
            v.extend(p);
            permutations.push(v);
        }
    }

    permutations
}

fn main() -> io::Result<()> {
    let file_path = "input.txt";
    let contents = fs::read_to_string(file_path)?;
    let memory: Vec<i32> = contents
        .trim()
        .split(',')
        .map(|s| s.parse().expect("Failed to parse intcode"))
        .collect();

    let part1_result = solve_part1(&memory);
    println!("Part 1 Result: {}", part1_result);

    let part2_result = solve_part2(&memory);
    println!("Part 2 Result: {}", part2_result);

    Ok(())
}
