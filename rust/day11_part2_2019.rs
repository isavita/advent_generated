
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct IntcodeComputer {
    memory: HashMap<i64, i64>,
    ip: i64,
    relative_base: i64,
    halted: bool,
}

impl IntcodeComputer {
    fn new(program: &[i64]) -> Self {
        let mut memory = HashMap::new();
        for (i, &val) in program.iter().enumerate() {
            memory.insert(i as i64, val);
        }
        Self {
            memory,
            ip: 0,
            relative_base: 0,
            halted: false,
        }
    }

    fn get_parameter(&self, mode: i64, offset: i64) -> i64 {
        let param = *self.memory.get(&(self.ip + offset)).unwrap_or(&0);
        match mode {
            0 => *self.memory.get(&param).unwrap_or(&0),
            1 => param,
            2 => *self.memory.get(&(self.relative_base + param)).unwrap_or(&0),
            _ => panic!("Unknown parameter mode: {}", mode),
        }
    }

    fn set_parameter(&mut self, mode: i64, offset: i64, value: i64) {
        let param = *self.memory.get(&(self.ip + offset)).unwrap_or(&0);
        let address = match mode {
            0 => param,
            2 => self.relative_base + param,
            _ => panic!("Unknown parameter mode for writing: {}", mode),
        };
        self.memory.insert(address, value);
    }

    fn run(&mut self) -> Vec<Option<i64>> {
        let mut outputs = Vec::new();
        let mut input_needed = false;

        loop {
            let instruction = *self.memory.get(&self.ip).unwrap_or(&0);
            let opcode = instruction % 100;
            let modes = [
                (instruction / 100) % 10,
                (instruction / 1000) % 10,
                (instruction / 10000) % 10,
            ];

            match opcode {
                1 => {
                    let param1 = self.get_parameter(modes[0], 1);
                    let param2 = self.get_parameter(modes[1], 2);
                    self.set_parameter(modes[2], 3, param1 + param2);
                    self.ip += 4;
                }
                2 => {
                    let param1 = self.get_parameter(modes[0], 1);
                    let param2 = self.get_parameter(modes[1], 2);
                    self.set_parameter(modes[2], 3, param1 * param2);
                    self.ip += 4;
                }
                3 => {
                    if input_needed {
                        outputs.push(None);
                        return outputs;
                    }
                    input_needed = true;
                }
                4 => {
                    let output_val = self.get_parameter(modes[0], 1);
                    self.ip += 2;
                    outputs.push(Some(output_val));
                }
                5 => {
                    let param1 = self.get_parameter(modes[0], 1);
                    let param2 = self.get_parameter(modes[1], 2);
                    if param1 != 0 {
                        self.ip = param2;
                    } else {
                        self.ip += 3;
                    }
                }
                6 => {
                    let param1 = self.get_parameter(modes[0], 1);
                    let param2 = self.get_parameter(modes[1], 2);
                    if param1 == 0 {
                        self.ip = param2;
                    } else {
                        self.ip += 3;
                    }
                }
                7 => {
                    let param1 = self.get_parameter(modes[0], 1);
                    let param2 = self.get_parameter(modes[1], 2);
                    self.set_parameter(modes[2], 3, (param1 < param2) as i64);
                    self.ip += 4;
                }
                8 => {
                    let param1 = self.get_parameter(modes[0], 1);
                    let param2 = self.get_parameter(modes[1], 2);
                    self.set_parameter(modes[2], 3, (param1 == param2) as i64);
                    self.ip += 4;
                }
                9 => {
                    let param1 = self.get_parameter(modes[0], 1);
                    self.relative_base += param1;
                    self.ip += 2;
                }
                99 => {
                    self.halted = true;
                    break;
                }
                _ => panic!("Unknown opcode: {}", opcode),
            }
             if input_needed && outputs.len()>0{
                 return outputs;
             }
        }
        outputs
    }

     fn provide_input(&mut self, input_val: i64) {
        let instruction = *self.memory.get(&self.ip).unwrap_or(&0);
        let modes = (instruction / 100) % 10;
        self.set_parameter(modes, 1, input_val);
        self.ip += 2;

    }
}

struct Robot {
    computer: IntcodeComputer,
    direction: i32,
    position: (i32, i32),
    panels: HashMap<(i32, i32), i64>,
    painted_panels: HashMap<(i32,i32),bool>,
}

impl Robot {
    fn new(program: &[i64], start_panel_color: i64) -> Self {
        let mut computer = IntcodeComputer::new(program);
        let mut panels = HashMap::new();
        let position = (0, 0);
        panels.insert(position, start_panel_color);
        Self {
            computer,
            direction: 0,
            position,
            panels,
            painted_panels: HashMap::new(),
        }
    }

    fn turn_and_move(&mut self, turn_direction: i64) {
        match turn_direction {
            0 => self.direction = (self.direction - 1 + 4) % 4,
            1 => self.direction = (self.direction + 1) % 4,
            _ => panic!("Unknown turn direction: {}", turn_direction),
        }

        let (x, y) = self.position;
        match self.direction {
            0 => self.position = (x, y - 1),
            1 => self.position = (x + 1, y),
            2 => self.position = (x, y + 1),
            3 => self.position = (x - 1, y),
            _ => unreachable!(),
        }
    }

    fn run(&mut self) {
       loop{
            let outputs = self.computer.run();
            let mut outputs_iter = outputs.into_iter();

            while let Some(output) = outputs_iter.next(){
                match output{
                    Some(paint_color) =>{
                        let turn_direction = outputs_iter.next().unwrap().unwrap();
                        self.panels.insert(self.position, paint_color);
                        self.painted_panels.insert(self.position,true);
                        self.turn_and_move(turn_direction);
                    }
                    None =>{
                        let current_color = *self.panels.get(&self.position).unwrap_or(&0);
                        self.computer.provide_input(current_color);
                    }
                }

            }

            if self.computer.halted{
                break;
            }
        }
    }

    fn get_painted_panels_count(&self) -> usize {
        self.painted_panels.len()
    }

    fn render_panels(&self) {
        if self.panels.is_empty() {
            println!("No panels painted.");
            return;
        }

        let min_x = self.panels.keys().map(|&(x, _)| x).min().unwrap();
        let max_x = self.panels.keys().map(|&(x, _)| x).max().unwrap();
        let min_y = self.panels.keys().map(|&(_, y)| y).min().unwrap();
        let max_y = self.panels.keys().map(|&(_, y)| y).max().unwrap();

        println!("Registration Identifier:");
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                if *self.panels.get(&(x, y)).unwrap_or(&0) == 1 {
                    print!("#");
                } else {
                    print!(" ");
                }
            }
            println!();
        }
    }
}

fn parse_input(file_path: &str) -> Vec<i64> {
    let file = File::open(file_path).unwrap();
    let reader = BufReader::new(file);
    reader
        .split(b',')
        .map(|line| line.unwrap().iter().map(|&c| c as char).collect::<String>().trim().parse().unwrap())
        .collect()
}

fn main() {
    let program = parse_input("input.txt");

    let mut robot_part1 = Robot::new(&program, 0);
    robot_part1.run();
    let painted_count_part1 = robot_part1.get_painted_panels_count();
    println!("Part One: {} panels painted at least once.", painted_count_part1);

    let mut robot_part2 = Robot::new(&program, 1);
    robot_part2.run();
    println!("Part Two: Registration identifier painted on the hull.");
    robot_part2.render_panels();
}
