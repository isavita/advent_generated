
use std::{
    collections::HashMap,
    fs::read_to_string,
    io::{self, BufRead},
    str::FromStr,
};

#[derive(Clone, Copy)]
enum Mode {
    Position,
    Immediate,
    Relative,
}

#[derive(Clone, Copy)]
enum Opcode {
    Add = 1,
    Mul,
    Input,
    Output,
    Jt,
    Jf,
    Lt,
    Eq,
    Rbo,
    Halt = 99,
}

fn decode(n: i64) -> (Opcode, [Mode; 3]) {
    let op = match n % 100 {
        1 => Opcode::Add,
        2 => Opcode::Mul,
        3 => Opcode::Input,
        4 => Opcode::Output,
        5 => Opcode::Jt,
        6 => Opcode::Jf,
        7 => Opcode::Lt,
        8 => Opcode::Eq,
        9 => Opcode::Rbo,
        99 => Opcode::Halt,
        _ => panic!("Unknown opcode: {}", n % 100),
    };
    let mut modes = [Mode::Position; 3];
    let mut n = n / 100;
    for i in 0..3 {
        modes[i] = match n % 10 {
            0 => Mode::Position,
            1 => Mode::Immediate,
            2 => Mode::Relative,
            _ => panic!("Unknown mode: {}", n % 10),
        };
        n /= 10;
    }
    (op, modes)
}

struct Machine {
    data: HashMap<i64, i64>,
    ip: i64,
    in_chan: Vec<i64>,
    out_chan: Vec<i64>,
    relbase: i64,
}

impl Machine {
    fn new(program: &[i64], in_chan: Vec<i64>) -> Self {
        let mut data = HashMap::new();
        for (i, &n) in program.iter().enumerate() {
            data.insert(i as i64, n);
        }
        Machine {
            data,
            ip: 0,
            in_chan,
            out_chan: Vec::new(),
            relbase: 0,
        }
    }

    fn get(&self, i: i64, mo: Mode) -> i64 {
        match mo {
            Mode::Immediate => *self.data.get(&i).unwrap_or(&0),
            Mode::Position => *self.data.get(&self.data.get(&i).unwrap_or(&0)).unwrap_or(&0),
            Mode::Relative => {
                *self
                    .data
                    .get(&(self.relbase + *self.data.get(&i).unwrap_or(&0)))
                    .unwrap_or(&0)
            }
        }
    }

    fn set(&mut self, i: i64, mo: Mode, val: i64) {
        match mo {
            Mode::Position => {
                let addr = *self.data.get(&i).unwrap_or(&0);
                self.data.insert(addr, val);
            }
            Mode::Relative => {
                 let addr = self.relbase + *self.data.get(&i).unwrap_or(&0);
                self.data.insert(addr, val);
            }
            Mode::Immediate => panic!("cannot set in immediate mode"),
        }
    }

    fn step(&mut self) -> bool {
        let (op, modes) = decode(*self.data.get(&self.ip).unwrap_or(&0));
        match op {
            Opcode::Add => {
                let val = self.get(self.ip + 1, modes[0]) + self.get(self.ip + 2, modes[1]);
                self.set(self.ip + 3, modes[2], val);
                self.ip += 4;
            }
            Opcode::Mul => {
                let val = self.get(self.ip + 1, modes[0]) * self.get(self.ip + 2, modes[1]);
                self.set(self.ip + 3, modes[2], val);
                self.ip += 4;
            }
            Opcode::Input => {
                let input = self.in_chan.remove(0);
                self.set(self.ip + 1, modes[0], input);
                self.ip += 2;
            }
            Opcode::Output => {
                let output = self.get(self.ip + 1, modes[0]);
                self.out_chan.push(output);
                self.ip += 2;
            }
            Opcode::Jt => {
                if self.get(self.ip + 1, modes[0]) != 0 {
                    self.ip = self.get(self.ip + 2, modes[1]);
                } else {
                    self.ip += 3;
                }
            }
            Opcode::Jf => {
                if self.get(self.ip + 1, modes[0]) == 0 {
                    self.ip = self.get(self.ip + 2, modes[1]);
                } else {
                    self.ip += 3;
                }
            }
            Opcode::Lt => {
                if self.get(self.ip + 1, modes[0]) < self.get(self.ip + 2, modes[1]) {
                    self.set(self.ip + 3, modes[2], 1);
                } else {
                    self.set(self.ip + 3, modes[2], 0);
                }
                self.ip += 4;
            }
            Opcode::Eq => {
                if self.get(self.ip + 1, modes[0]) == self.get(self.ip + 2, modes[1]) {
                    self.set(self.ip + 3, modes[2], 1);
                } else {
                    self.set(self.ip + 3, modes[2], 0);
                }
                self.ip += 4;
            }
            Opcode::Rbo => {
                self.relbase += self.get(self.ip + 1, modes[0]);
                self.ip += 2;
            }
            Opcode::Halt => return false,
        }
        true
    }

    fn run(&mut self) {
        while self.step() {}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn add(&self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Dir {
    N,
    E,
    S,
    W,
}

impl Dir {
    fn point(&self) -> Point {
        match self {
            Dir::N => Point { x: 0, y: 1 },
            Dir::E => Point { x: 1, y: 0 },
            Dir::S => Point { x: 0, y: -1 },
            Dir::W => Point { x: -1, y: 0 },
        }
    }
    fn point_r(&self) -> Point {
        match self {
            Dir::N => Point { x: 0, y: -1 },
            Dir::E => Point { x: 1, y: 0 },
            Dir::S => Point { x: 0, y: 1 },
            Dir::W => Point { x: -1, y: 0 },
        }
    }
    fn next(&self) -> Dir {
        match self {
            Dir::N => Dir::E,
            Dir::E => Dir::S,
            Dir::S => Dir::W,
            Dir::W => Dir::N,
        }
    }
      fn prev(&self) -> Dir {
        match self {
            Dir::N => Dir::W,
            Dir::E => Dir::N,
            Dir::S => Dir::E,
            Dir::W => Dir::S,
        }
    }
}
fn from_byte(b: u8) -> Dir {
    match b {
        b'N' | b'U' | b'^' => Dir::N,
        b'E' | b'R' | b'>' => Dir::E,
        b'S' | b'D' | b'v' => Dir::S,
        b'W' | b'L' | b'<' => Dir::W,
        _ => panic!("Unknown dir byte"),
    }
}
fn parse(program: &[i64]) -> (HashMap<Point, ()>, Point, Dir) {
    let mut machine = Machine::new(program, vec![]);
    machine.run();
    let output = machine.out_chan;
    let output_str: String = output.iter().map(|&x| x as u8 as char).collect();

    let mut scaffolding = HashMap::new();
    let mut robot = Point { x: 0, y: 0 };
    let mut dir = Dir::N;
    let mut y = 0;

    for line in output_str.lines() {
        for (x, c) in line.chars().enumerate() {
            let x = x as i64;
            match c {
                '^' | 'v' | '<' | '>' => {
                    robot = Point { x, y };
                    dir = from_byte(c as u8);
                     scaffolding.insert(Point { x, y }, ());

                }
                '#' => {
                     scaffolding.insert(Point { x, y }, ());
                }
                _ => {}
            }
        }
        y+=1;
    }
    (scaffolding, robot, dir)
}
fn path(scaffolding: &HashMap<Point,()>, mut robot: Point, mut dir: Dir) -> String {
    let mut sections = Vec::new();
    let mut dist = 0;
    let mut d = ' ';
    loop {
         if scaffolding.contains_key(&robot.add(dir.point_r())) {
            robot = robot.add(dir.point_r());
            dist += 1;
           
            continue;
        }
        if dist > 0 {
             sections.push(format!("{},{}", d,dist));
         }
        if scaffolding.contains_key(&robot.add(dir.next().point_r())) {
            robot = robot.add(dir.next().point_r());
            dir = dir.next();
            dist = 1;
            d = 'R';
        } else if scaffolding.contains_key(&robot.add(dir.prev().point_r())) {
            robot = robot.add(dir.prev().point_r());
            dir = dir.prev();
            dist = 1;
            d = 'L';
        } else {
            break;
        }
    }
    sections.join(",")
}

fn encode(path: &str) -> (String, String, String, String) {
    let mut a = String::new();
    let mut b = String::new();
    let mut c = String::new();
    let mut seq = String::new();
    let mut path_c = path.to_string() + ",";
    'outer: for i in 2..=21{
         for j in 2..=21{
            for k in 2..=21 {
                let mut next = path_c.clone();
                a = next[..i].to_string();
                next = next.replace(&a, "");
                b = next[..j].to_string();
                next = next.replace(&b, "");
                c = next[..k].to_string();
                next = next.replace(&c,"");
                 if next == "" {
                     break 'outer;
                 }
             }
         }
    }
    a = a.trim_end_matches(',').to_string();
     b = b.trim_end_matches(',').to_string();
     c = c.trim_end_matches(',').to_string();

    seq = path.to_string();
    seq = seq.replace(&a,"A");
    seq = seq.replace(&b, "B");
    seq = seq.replace(&c, "C");

    (seq,a,b,c)
}
fn dust(program: &mut Vec<i64>, scaffolding: &HashMap<Point,()>, robot: Point, dir: Dir) -> i64 {
    let (seq,a,b,c) = encode(&path(scaffolding, robot, dir));
    let input = format!("{seq}\n{a}\n{b}\n{c}\nn\n");
    let input_chars: Vec<i64> = input.chars().map(|c| c as i64).collect();
    program[0] = 2;
    let mut machine = Machine::new(program, input_chars);
    machine.run();
    *machine.out_chan.last().unwrap()
}
fn main() -> io::Result<()> {
    let input = read_to_string("input.txt")?;
    let program: Vec<i64> = input
        .trim()
        .split(',')
        .map(|s| i64::from_str(s).unwrap())
        .collect();

    let (scaffolding, robot, dir) = parse(&program);
     println!("{}",dust(&mut program.clone(), &scaffolding, robot, dir));
    Ok(())
}
