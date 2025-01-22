
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct P {
    x: i32,
    y: i32,
}

#[derive(Clone, Copy)]
enum Dir {
    N,
    E,
    S,
    W,
}

impl Dir {
    fn rotate(self, direction: char) -> Self {
        match direction {
            'R' => match self {
                Dir::N => Dir::E,
                Dir::E => Dir::S,
                Dir::S => Dir::W,
                Dir::W => Dir::N,
            },
            'L' => match self {
                Dir::N => Dir::W,
                Dir::E => Dir::N,
                Dir::S => Dir::E,
                Dir::W => Dir::S,
            },
            _ => self,
        }
    }
    fn points(self) -> i32 {
        match self {
            Dir::N => 3,
            Dir::E => 0,
            Dir::S => 1,
            Dir::W => 2,
        }
    }
}

struct Movement {
    steps: i32,
    rotate: char,
}

struct Human {
    curr: P,
    facing: Dir,
}

fn main() {
    let (map, size, movements) = parse_input();

    let mut human = Human {
        curr: P { x: 0, y: size },
        facing: Dir::E,
    };

    for mov in movements {
        human.facing = human.facing.rotate(mov.rotate);
        for _ in 0..mov.steps {
            if !walk(&mut human, &map, size) {
                break;
            }
        }
    }

    println!(
        "{}",
        1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points()
    );
}

fn parse_input() -> (HashMap<P, bool>, i32, Vec<Movement>) {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let mut map = HashMap::new();
    let mut size = 0;
    let mut line_count = 0;

    for line in &mut lines {
        let line = line.unwrap();
         if line.is_empty() {
            break;
        }
        if line_count == 0 {
            size = (line.len() / 3) as i32;
        }
        for (c, char) in line.chars().enumerate() {
            match char {
                ' ' => continue,
                '#' => {
                    map.insert(P { x: line_count, y: c as i32 }, true);
                }
                '.' => {
                    map.insert(P { x: line_count, y: c as i32 }, false);
                }
                _ => {}
            }
        }
        line_count+=1;
    }
    
    let path_line = lines.next().unwrap().unwrap();

    let movements = parse_path(&path_line);
    (map, size, movements)
}

fn parse_path(path: &str) -> Vec<Movement> {
    let mut movements = Vec::new();
    let mut acc = 0;
    for char in path.chars() {
        match char {
            'R' => {
                movements.push(Movement { steps: acc, rotate: '\0'});
                acc = 0;
                movements.push(Movement { steps: 0, rotate: 'R' });
            }
            'L' => {
                movements.push(Movement { steps: acc, rotate: '\0'});
                acc = 0;
                movements.push(Movement { steps: 0, rotate: 'L' });
            }
            '0'..='9' => {
                acc = 10 * acc + (char as i32 - '0' as i32);
            }
            _ => {}
        }
    }
        movements.push(Movement { steps: acc, rotate: '\0'});
    movements
}

fn walk(human: &mut Human, map: &HashMap<P, bool>, size: i32) -> bool {
    let dir_delta = match human.facing {
        Dir::N => P { x: -1, y: 0 },
        Dir::E => P { x: 0, y: 1 },
        Dir::S => P { x: 1, y: 0 },
        Dir::W => P { x: 0, y: -1 },
    };
    let next = P {
        x: human.curr.x + dir_delta.x,
        y: human.curr.y + dir_delta.y,
    };

    if let Some(wall) = map.get(&next) {
        if *wall {
            return false;
        }
        human.curr = next;
        return true;
    }

     let (next_pos, next_facing) = cross_border(next, human.facing, size);
    if  map.get(&next_pos).map_or(false, |&wall| wall) {
         return false;
     }

    human.curr = next_pos;
    human.facing = next_facing;
    true
}


fn cross_border(n: P, dir: Dir, size: i32) -> (P, Dir) {
    let x = n.x;
    let y = n.y;

    match (x, y, dir) {
        (-1, y, _) if y < 2 * size => (P { x: y + 2 * size, y: x + 1 }, Dir::E),
        (-1, y, _) if y >= 2 * size => (P { x: x + 4 * size, y: y - 2 * size }, Dir::N),
        (x,y,Dir::S) if x == size =>(P { x: y - size, y: x + size -1 }, Dir::W),
        (x,y,Dir::N) if x == 2 * size - 1 =>(P { x: y + size, y: x - size + 1 }, Dir::E),
        (x,y,Dir::S) if x == 3 * size => (P { x: y + 2 * size, y: x - 2 * size - 1 }, Dir::W),
        (x, y, _) if x == 4 * size => (P { x: x - 4 * size, y: y + 2 * size }, Dir::S),
         (x, y, _) if y == -1 && x < 3*size => (P { x: 3 * size - 1 - x, y: y + size + 1 }, Dir::E),
         (x, y, _) if y == -1 && x >= 3*size => (P { x: y + 1, y: x - 2 * size }, Dir::S),
        (x,y,_) if y == size -1 && x < size =>(P { x: 3 * size - 1 - x, y: y - size + 1 }, Dir::E),
        (x,y,Dir::W) if y == size -1 && x >= size =>(P { x: y + size + 1, y: x - size }, Dir::S),
         (x,y,Dir::E) if y == size =>(P { x: y + 2 * size - 1, y: x - 2 * size }, Dir::N),
        (x,y,Dir::E) if y == 2 * size && x < 2 * size => (P { x: y - size - 1, y: x + size }, Dir::N),
        (x,y,_) if y == 2 * size && x >= 2 * size => (P { x: 3 * size - 1 - x, y: y + size - 1 }, Dir::W),
       (x,y,_) if y == 3 * size =>(P { x: 3 * size - 1 - x, y: y - size - 1 }, Dir::W),
        _ => panic!("not a border crossing"),
    }
}
