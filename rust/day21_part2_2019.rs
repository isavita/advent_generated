
use std::collections::HashMap;
use std::fs;

struct VM {
    code: HashMap<i64, i64>,
    ip: i64,
    input: Vec<i64>,
    output: Vec<i64>,
    relative_base: i64,
}

impl VM {
    fn new(filename: &str) -> VM {
        let mut vm = VM {
            code: HashMap::new(),
            ip: 0,
            input: Vec::new(),
            output: Vec::new(),
            relative_base: 0,
        };
        vm.load(filename);
        vm
    }

    fn load(&mut self, filename: &str) {
        let contents = fs::read_to_string(filename).unwrap();
        for (i, val) in contents.trim().split(',').enumerate() {
            self.code.insert(i as i64, val.parse().unwrap());
        }
    }

    fn run(&mut self) {
        loop {
            let cmd = *self.code.get(&self.ip).unwrap_or(&0);
            let opcode = cmd % 100;
            let modes = [(cmd / 100) % 10, (cmd / 1000) % 10, (cmd / 10000) % 10];

            let get_param = |index: usize| -> i64 {
                let mode = modes[index - 1];
                let value = *self.code.get(&(self.ip + index as i64)).unwrap_or(&0);
                match mode {
                    0 => *self.code.get(&value).unwrap_or(&0),
                    1 => value,
                    2 => *self.code.get(&(self.relative_base + value)).unwrap_or(&0),
                    _ => panic!("Invalid mode"),
                }
            };

            let get_address = |index: usize| -> i64 {
                let mode = modes[index - 1];
                let value = *self.code.get(&(self.ip + index as i64)).unwrap_or(&0);
                match mode {
                    2 => self.relative_base + value,
                    _ => value,
                }
            };

            match opcode {
                1 => {
                    let address = get_address(3);
                    self.code.insert(address, get_param(1) + get_param(2));
                    self.ip += 4;
                }
                2 => {
                    let address = get_address(3);
                    self.code.insert(address, get_param(1) * get_param(2));
                    self.ip += 4;
                }
                3 => {
                    let address = get_address(1);
                    self.code.insert(address, self.input.remove(0));
                    self.ip += 2;
                }
                4 => {
                    self.output.push(get_param(1));
                    self.ip += 2;
                }
                5 => {
                    if get_param(1) != 0 {
                        self.ip = get_param(2);
                    } else {
                        self.ip += 3;
                    }
                }
                6 => {
                    if get_param(1) == 0 {
                        self.ip = get_param(2);
                    } else {
                        self.ip += 3;
                    }
                }
                7 => {
                    let address = get_address(3);
                    self.code.insert(address, if get_param(1) < get_param(2) { 1 } else { 0 });
                    self.ip += 4;
                }
                8 => {
                    let address = get_address(3);
                    self.code.insert(address, if get_param(1) == get_param(2) { 1 } else { 0 });
                    self.ip += 4;
                }
                9 => {
                    self.relative_base += get_param(1);
                    self.ip += 2;
                }
                99 => break,
                _ => panic!("Unknown opcode {}", opcode),
            }
        }
    }

    fn send_string(&mut self, s: &str) {
        for c in s.chars() {
            self.input.push(c as i64);
        }
        self.input.push(10);
    }
}

fn main() {
    let mut vm = VM::new("input.txt");
    let instructions = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT A T",
        "AND A T",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN",
    ];
    for i in instructions.iter() {
        vm.send_string(i);
    }
    vm.run();
    for output in vm.output {
        println!("{}", output);
    }
}
