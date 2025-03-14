
use std::collections::HashMap;
use std::fs;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Complex {
    x: i32,
    y: i32,
}

impl Complex {
    fn new(x: i32, y: i32) -> Self {
        Complex { x, y }
    }

    fn add(&self, other: &Complex) -> Complex {
        Complex {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

fn try_to_step(
    m: &mut HashMap<Complex, char>,
    pos: Complex,
    dir: Complex,
) -> bool {
    let orig = m.clone();
    match m.get(&pos) {
        Some('.') => return true,
        Some('O') | Some('@') => {
            if try_to_step(m, pos.add(&dir), dir) {
                let val = *m.get(&pos).unwrap();
                m.insert(pos.add(&dir), val);
                m.insert(pos, '.');
                return true;
            }
        }
        Some(']') => {
            if try_to_step(m, pos.add(&Complex::new(-1, 0)), dir) {
                return true;
            }
        }
        Some('[') => {
            if dir == Complex::new(-1, 0) {
                if try_to_step(m, pos.add(&Complex::new(-1, 0)), dir) {
                    m.insert(pos.add(&Complex::new(-1, 0)), '[');
                    m.insert(pos, ']');
                    m.insert(pos.add(&Complex::new(1, 0)), '.');
                    return true;
                }
            } else if dir == Complex::new(1, 0) {
                if try_to_step(m, pos.add(&Complex::new(2, 0)), dir) {
                    m.insert(pos, '.');
                    m.insert(pos.add(&Complex::new(1, 0)), '[');
                    m.insert(pos.add(&Complex::new(2, 0)), ']');
                    return true;
                }
            } else {
                if try_to_step(m, pos.add(&dir), dir)
                    && try_to_step(
                        m,
                        pos.add(&Complex::new(1, 0)).add(&dir),
                        dir,
                    )
                {
                    m.insert(pos, '.');
                    m.insert(pos.add(&Complex::new(1, 0)), '.');
                    m.insert(pos.add(&dir), '[');
                    m.insert(
                        pos.add(&dir).add(&Complex::new(1, 0)),
                        ']',
                    );
                    return true;
                }
            }
        }
        _ => {}
    }
    m.clear();
    m.extend(orig);
    false
}

fn solve(input_str: &str) -> i32 {
    let blocks: Vec<&str> =
        input_str.trim().split("\n\n").collect();
    let lines: Vec<&str> = blocks[0].split('\n').collect();
    let mut m: HashMap<Complex, char> = HashMap::new();
    for (y, row) in lines.iter().enumerate() {
        for (x, char) in row.chars().enumerate() {
            m.insert(Complex::new(x as i32, y as i32), char);
        }
    }
    let mut steps: Vec<Complex> = Vec::new();
    for char in blocks[1].replace('\n', "").chars() {
        match char {
            '^' => steps.push(Complex::new(0, -1)),
            '<' => steps.push(Complex::new(-1, 0)),
            '>' => steps.push(Complex::new(1, 0)),
            'v' => steps.push(Complex::new(0, 1)),
            _ => {}
        }
    }

    let mut robot: Complex =
        *m.iter().find(|(_, &v)| v == '@').map(|(k, _)| k).unwrap();

    for dir in steps {
        if try_to_step(&mut m, robot, dir) {
            robot = robot.add(&dir);
        }
    }
    m.iter()
        .filter(|(_, &v)| v == '[' || v == 'O')
        .map(|(k, _)| k.x + 100 * k.y)
        .sum()
}

fn scale_up(input_str: &str) -> String {
    input_str
        .replace('#', "##")
        .replace('.', "..")
        .replace('O', "[]")
        .replace('@', "@.")
}

fn main() {
    let input_str = fs::read_to_string("input.txt").unwrap();
    println!("{}", solve(&input_str));
    println!("{}", solve(&scale_up(&input_str)));
}
