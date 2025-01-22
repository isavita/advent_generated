
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    fn move_dir(&self, dir: char) -> Coord {
        match dir {
            'N' => Coord { x: self.x, y: self.y + 1 },
            'S' => Coord { x: self.x, y: self.y - 1 },
            'E' => Coord { x: self.x + 1, y: self.y },
            'W' => Coord { x: self.x - 1, y: self.y },
            _ => *self,
        }
    }
}

fn parse_regex(regex: &str) -> HashMap<Coord, Vec<Coord>> {
    let mut map: HashMap<Coord, Vec<Coord>> = HashMap::new();
    let mut stack: Vec<Coord> = Vec::new();
    let mut current_pos = Coord { x: 0, y: 0 };

    for c in regex.chars() {
        match c {
            '^' => {}
            '$' => {}
            'N' | 'S' | 'E' | 'W' => {
                let next_pos = current_pos.move_dir(c);
                map.entry(current_pos)
                   .or_insert_with(Vec::new)
                   .push(next_pos);
                map.entry(next_pos)
                   .or_insert_with(Vec::new)
                   .push(current_pos);
                current_pos = next_pos;
            }
            '(' => {
                stack.push(current_pos);
            }
            '|' => {
                current_pos = *stack.last().unwrap();
            }
            ')' => {
                current_pos = stack.pop().unwrap();
            }
            _ => {}
        }
    }
    map
}

fn bfs(map: &HashMap<Coord, Vec<Coord>>, start: Coord) -> HashMap<Coord, i32> {
    let mut distances: HashMap<Coord, i32> = HashMap::new();
    let mut queue: VecDeque<(Coord, i32)> = VecDeque::new();
    let mut visited: HashMap<Coord, bool> = HashMap::new();

    distances.insert(start, 0);
    queue.push_back((start, 0));
    visited.insert(start, true);

    while let Some((current, dist)) = queue.pop_front() {
        if let Some(neighbors) = map.get(&current) {
            for neighbor in neighbors {
                if !visited.contains_key(neighbor){
                    distances.insert(*neighbor, dist + 1);
                    queue.push_back((*neighbor, dist + 1));
                    visited.insert(*neighbor, true);
                }
            }
        }
    }
    distances
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let regex = reader.lines().next().unwrap()?;
    let map = parse_regex(&regex);
    let distances = bfs(&map, Coord{x:0, y:0});

    let max_distance = distances.values().max().unwrap_or(&0);
    println!("Part 1: Furthest room is {} doors away", max_distance);

    let far_rooms = distances.values().filter(|&d| *d >= 1000).count();
    println!("Part 2: {} rooms are at least 1000 doors away", far_rooms);
    
    Ok(())
}
