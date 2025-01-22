
use std::fs::File;
use std::io::{self, BufRead, Write};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let line = reader.lines().next().unwrap()?;
    let program: Vec<i64> = line
        .split(',')
        .map(|s| s.trim().parse().unwrap())
        .collect();

    let instructions = vec![
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK",
    ];

    let mut input_buffer: Vec<i64> = instructions
        .iter()
        .flat_map(|s| s.chars().map(|c| c as i64).chain(std::iter::once(10)))
        .collect();
    
    let mut output_buffer: Vec<i64> = Vec::new();
    let mut computer = IntCodeComputer::new(program);
    
    let result = computer.run_with_io(&mut input_buffer, &mut output_buffer);

    match result {
        Ok(()) => {
            if let Some(damage) = output_buffer.last() {
                if *damage > 127 {
                  println!("{}", damage);
                } else {
                    for char_val in output_buffer {
                        print!("{}", char::from_u32(char_val as u32).unwrap());
                    }
                }
            } else {
                println!("No output from the program.");
            }
        }
        Err(err) => {
            eprintln!("Program error: {}", err);
        }
    }

    Ok(())
}

#[derive(Debug)]
struct IntCodeComputer {
    memory: Vec<i64>,
    pc: usize,
    relative_base: i64,
}

impl IntCodeComputer {
    fn new(program: Vec<i64>) -> Self {
        IntCodeComputer {
            memory: program,
            pc: 0,
            relative_base: 0,
        }
    }

    fn get_parameter_mode(&self, opcode: i64, param_num: usize) -> i64 {
        let mode = (opcode / (10_i64.pow((param_num + 1) as u32))) % 10;
        mode
    }

    fn get_parameter_address(&mut self, param_num: usize, opcode: i64) -> usize {
        let mode = self.get_parameter_mode(opcode, param_num);
        let addr = self.memory[self.pc + param_num] as usize;
        match mode {
            0 => addr,
            1 => self.pc + param_num,
            2 => (self.relative_base + self.memory[self.pc + param_num]) as usize,
             _ => panic!("Invalid parameter mode: {}", mode),
        }
    }

    fn get_value(&mut self, param_num: usize, opcode: i64) -> i64 {
        let addr = self.get_parameter_address(param_num, opcode);
        self.memory.get(addr).cloned().unwrap_or(0)
    }

    fn set_value(&mut self, param_num: usize, opcode: i64, value: i64) {
        let addr = self.get_parameter_address(param_num, opcode);
        if addr >= self.memory.len() {
            self.memory.resize(addr + 1, 0);
        }
        self.memory[addr] = value;
    }

    fn run_with_io(
        &mut self,
        input_buffer: &mut Vec<i64>,
        output_buffer: &mut Vec<i64>,
    ) -> Result<(), String> {
        loop {
            let opcode = self.memory[self.pc] % 100;
            match opcode {
                1 => {
                    let value1 = self.get_value(1, self.memory[self.pc]);
                    let value2 = self.get_value(2, self.memory[self.pc]);
                    self.set_value(3, self.memory[self.pc], value1 + value2);
                    self.pc += 4;
                }
                2 => {
                    let value1 = self.get_value(1, self.memory[self.pc]);
                    let value2 = self.get_value(2, self.memory[self.pc]);
                    self.set_value(3, self.memory[self.pc], value1 * value2);
                    self.pc += 4;
                }
                3 => {
                    if input_buffer.is_empty() {
                        return Err("Input buffer is empty".to_string());
                    }
                    let input = input_buffer.remove(0);
                    self.set_value(1, self.memory[self.pc], input);
                    self.pc += 2;
                }
                4 => {
                    let output = self.get_value(1, self.memory[self.pc]);
                    output_buffer.push(output);
                    self.pc += 2;
                }
                5 => {
                    let value1 = self.get_value(1, self.memory[self.pc]);
                    if value1 != 0 {
                        self.pc = self.get_value(2, self.memory[self.pc]) as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                6 => {
                    let value1 = self.get_value(1, self.memory[self.pc]);
                    if value1 == 0 {
                        self.pc = self.get_value(2, self.memory[self.pc]) as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                7 => {
                    let value1 = self.get_value(1, self.memory[self.pc]);
                    let value2 = self.get_value(2, self.memory[self.pc]);
                    self.set_value(3, self.memory[self.pc], if value1 < value2 { 1 } else { 0 });
                    self.pc += 4;
                }
                8 => {
                    let value1 = self.get_value(1, self.memory[self.pc]);
                    let value2 = self.get_value(2, self.memory[self.pc]);
                    self.set_value(3, self.memory[self.pc], if value1 == value2 { 1 } else { 0 });
                    self.pc += 4;
                }
                9 => {
                    self.relative_base += self.get_value(1, self.memory[self.pc]);
                    self.pc += 2;
                }
                99 => break Ok(()),
                 _ => return Err(format!("Unknown opcode: {}", opcode)),
             }
         }
    }
}
