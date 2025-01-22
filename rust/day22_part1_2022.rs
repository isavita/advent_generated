
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct P {
    x: i32,
    y: i32,
}

#[derive(Clone, Copy)]
enum Dir {
    N, E, S, W
}

impl Dir {
    fn rotate(&self, direction: char) -> Self {
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
            }
            _ => *self,
        }
    }

     fn points(&self) -> i32 {
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

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines();

    let mut map: HashMap<P, bool> = HashMap::new();
    let mut size = 0;

    let mut r = 0;
    while let Some(Ok(line)) = lines.next() {
        if line.is_empty() {
            break;
        }

        if r == 0 {
             size = line.len() as i32/ 3;
        }

        for (c, char) in line.chars().enumerate() {
            match char {
                ' ' => continue,
                '#' => { map.insert(P{x: r as i32, y: c as i32}, true); },
                '.' => { map.insert(P{x: r as i32, y: c as i32}, false); },
                _ => {},
            }
        }
        r += 1;
    }
    let path = lines.next().unwrap()?;
    let movements = parse_path(path);
    
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
     println!("{}", 1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points());

    Ok(())
}

fn parse_path(path: String) -> Vec<Movement> {
    let mut movements = Vec::new();
    let mut acc = 0;

    for char in path.chars() {
        match char {
            'R' => {
                movements.push(Movement { steps: acc, rotate: '\0'});
                 acc = 0;
                 movements.push(Movement { steps: 0, rotate: 'R'});
            },
            'L' => {
                  movements.push(Movement { steps: acc, rotate: '\0'});
                 acc = 0;
                 movements.push(Movement { steps: 0, rotate: 'L'});
            },
            _ => {
                 acc = acc * 10 + (char as i32 - '0' as i32);
            }
        }
    }
     movements.push(Movement{steps: acc, rotate: '\0'});
    movements
}

fn walk(human: &mut Human, map: &HashMap<P,bool>, size: i32) -> bool {
    let dir = match human.facing {
        Dir::N => P { x: -1, y: 0 },
        Dir::E => P { x: 0, y: 1 },
        Dir::S => P { x: 1, y: 0 },
        Dir::W => P { x: 0, y: -1 },
    };

    let mut next = P { x: human.curr.x + dir.x, y: human.curr.y + dir.y };
     if let Some(wall) = map.get(&next){
         if *wall {
            return false;
        }
         human.curr = next;
        return true;
     }

     let opp_dir = P{x: -dir.x, y: -dir.y};
     loop {
        let look_ahead = P { x: next.x + opp_dir.x, y: next.y + opp_dir.y };
        if !map.contains_key(&look_ahead) {
            if  *map.get(&next).unwrap() {
                return false;
            }
            human.curr = next;
            return true;
        }
        next = look_ahead;
     }
}
