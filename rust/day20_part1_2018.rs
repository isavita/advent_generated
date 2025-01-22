
use std::{
    collections::{HashMap, VecDeque},
    fs::read_to_string,
};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let regex = read_to_string("input.txt").unwrap();
    let dm = build_map(&regex[1..regex.len() - 1]);
    let max_doors = find_furthest_room(&dm);
    println!("{}", max_doors);
}

fn build_map(regex: &str) -> HashMap<Point, Vec<Point>> {
    let mut dm: HashMap<Point, Vec<Point>> = HashMap::new();
    let mut stack: Vec<Point> = Vec::new();
    let mut cp = Point { x: 0, y: 0 };

    for c in regex.chars() {
        match c {
            '(' => {
                stack.push(cp);
            }
            '|' => {
                cp = *stack.last().unwrap();
            }
            ')' => {
                cp = stack.pop().unwrap();
            }
            _ => {
                let np = move_point(cp, c);
                dm.entry(cp).or_insert(Vec::new()).push(np);
                cp = np;
            }
        }
    }
    dm
}

fn move_point(p: Point, dir: char) -> Point {
    match dir {
        'N' => Point { x: p.x, y: p.y - 1 },
        'S' => Point { x: p.x, y: p.y + 1 },
        'E' => Point { x: p.x + 1, y: p.y },
        'W' => Point { x: p.x - 1, y: p.y },
        _ => p,
    }
}

fn find_furthest_room(dm: &HashMap<Point, Vec<Point>>) -> usize {
    let mut visited: HashMap<Point, usize> = HashMap::new();
    let mut queue: VecDeque<Point> = VecDeque::new();
    queue.push_back(Point { x: 0, y: 0 });
    visited.insert(Point { x: 0, y: 0 }, 0);
    let mut max_doors = 0;

    while let Some(p) = queue.pop_front() {
        if let Some(neighbors) = dm.get(&p) {
           for &np in neighbors{
                if !visited.contains_key(&np) {
                    let dist = visited.get(&p).unwrap() + 1;
                    visited.insert(np, dist);
                    max_doors = max_doors.max(dist);
                    queue.push_back(np);
                }
            }
        }
    }
    max_doors
}
