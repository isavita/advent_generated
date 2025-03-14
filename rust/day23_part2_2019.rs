
use std::collections::{HashMap, VecDeque};
use std::fs;

#[derive(Clone)]
struct IntcodeComputer {
    memory: HashMap<usize, i64>,
    ip: usize,
    relative_base: i64,
    inputs: VecDeque<i64>,
    outputs: VecDeque<i64>,
    halted: bool,
    idle: bool,
}

impl IntcodeComputer {
    fn new(program: &[i64], inputs: Vec<i64>) -> Self {
        let mut memory = HashMap::new();
        for (i, &val) in program.iter().enumerate() {
            memory.insert(i, val);
        }
        Self {
            memory,
            ip: 0,
            relative_base: 0,
            inputs: inputs.into(),
            outputs: VecDeque::new(),
            halted: false,
            idle: false,
        }
    }

    fn get_param(&self, mode: i64, offset: usize) -> i64 {
        let address = self.memory.get(&(self.ip + offset)).copied().unwrap_or(0);
        match mode {
            0 => self.memory.get(&(address as usize)).copied().unwrap_or(0),
            1 => address,
            2 => self
                .memory
                .get(&((self.relative_base + address) as usize))
                .copied()
                .unwrap_or(0),
            _ => panic!("Unknown mode {}", mode),
        }
    }

    fn set_param(&mut self, mode: i64, offset: usize, value: i64) {
        let address = self.memory.get(&(self.ip + offset)).copied().unwrap_or(0);
        let target = match mode {
            0 => address as usize,
            2 => (self.relative_base + address) as usize,
            _ => panic!("Unknown mode {}", mode),
        };
        *self.memory.entry(target).or_insert(0) = value;
    }

    fn run(&mut self) {
        while !self.halted {
            let opcode = self.memory.get(&self.ip).copied().unwrap_or(0) % 100;
            let modes = [
                (self.memory.get(&self.ip).copied().unwrap_or(0) / 100) % 10,
                (self.memory.get(&self.ip).copied().unwrap_or(0) / 1000) % 10,
                (self.memory.get(&self.ip).copied().unwrap_or(0) / 10000) % 10,
            ];

            match opcode {
                99 => {
                    self.halted = true;
                    return;
                }
                1 | 2 | 7 | 8 => {
                    let param1 = self.get_param(modes[0], 1);
                    let param2 = self.get_param(modes[1], 2);
                    let result = match opcode {
                        1 => param1 + param2,
                        2 => param1 * param2,
                        7 => (param1 < param2) as i64,
                        8 => (param1 == param2) as i64,
                        _ => unreachable!(),
                    };
                    self.set_param(modes[2], 3, result);
                    self.ip += 4;
                }
                3 => {
                    if let Some(value) = self.inputs.pop_front() {
                        self.set_param(modes[0], 1, value);
                        self.ip += 2;
                        self.idle = false;
                    } else {
                        self.set_param(modes[0], 1, -1);
                        self.ip += 2;
                        self.idle = true;
                        return;
                    }
                }
                4 => {
                    let param1 = self.get_param(modes[0], 1);
                    self.outputs.push_back(param1);
                    self.ip += 2;
                    self.idle = false;
                    if self.outputs.len() == 3 {
                        return;
                    }
                }
                5 | 6 => {
                    let param1 = self.get_param(modes[0], 1);
                    let param2 = self.get_param(modes[1], 2);
                    if (opcode == 5 && param1 != 0) || (opcode == 6 && param1 == 0) {
                        self.ip = param2 as usize;
                    } else {
                        self.ip += 3;
                    }
                }
                9 => {
                    let param1 = self.get_param(modes[0], 1);
                    self.relative_base += param1;
                    self.ip += 2;
                }
                _ => panic!("Unknown opcode {}", opcode),
            }
        }
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let program: Vec<i64> = input
        .trim()
        .split(',')
        .map(|x| x.parse().expect("Failed to parse"))
        .collect();

    let mut computers: Vec<_> = (0..50)
        .map(|address| IntcodeComputer::new(&program, vec![address]))
        .collect();

    let mut packet_queues: Vec<VecDeque<(i64, i64)>> = vec![VecDeque::new(); 50];
    let mut nat_packet = None;
    let mut prev_nat_y = None;

    loop {
        for (i, computer) in computers.iter_mut().enumerate() {
            if let Some(&(x, y)) = packet_queues[i].front() {
                computer.inputs.extend(vec![x, y]);
                packet_queues[i].pop_front();
                computer.idle = false;
            } else {
                computer.inputs.push_back(-1);
            }

            computer.run();

            while computer.outputs.len() >= 3 {
                let dest = computer.outputs.pop_front().unwrap();
                let x = computer.outputs.pop_front().unwrap();
                let y = computer.outputs.pop_front().unwrap();

                if dest == 255 {
                    nat_packet = Some((x, y));
                } else if (0..50).contains(&dest) {
                    packet_queues[dest as usize].push_back((x, y));
                }
            }
        }

        if packet_queues.iter().all(|q| q.is_empty())
            && computers.iter().all(|c| c.idle)
            && nat_packet.is_some()
        {
            let (x, y) = nat_packet.unwrap();
            packet_queues[0].push_back((x, y));
            if let Some(prev_y) = prev_nat_y {
                if y == prev_y {
                    println!("{}", y);
                    return;
                }
            }
            prev_nat_y = Some(y);
        }
    }
}
