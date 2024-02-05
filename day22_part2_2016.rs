
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }

    fn neighbors(&self) -> Vec<Point> {
        vec![
            Point::new(self.x, self.y + 1),
            Point::new(self.x, self.y - 1),
            Point::new(self.x + 1, self.y),
            Point::new(self.x - 1, self.y),
        ]
    }
}

#[derive(Debug, Clone)]
struct Node {
    used: i32,
    avail: i32,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: i32,
    point: Point,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    let content = fs::read_to_string("input.txt").expect("Failed to read file");
    let mut nodes = HashMap::new();
    for line in content.lines().skip(2) {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let coords = parts[0]
            .split('-')
            .filter_map(|x| i32::from_str(x.trim_matches(|p: char| !p.is_ascii_digit())).ok())
            .collect::<Vec<i32>>();
        let used = parts[2][..parts[2].len() - 1].parse::<i32>().unwrap();
        let avail = parts[3][..parts[3].len() - 1].parse::<i32>().unwrap();
        nodes.insert(Point::new(coords[0], coords[1]), Node { used, avail });
    }

    println!("{}", min_moves(&nodes));
}

fn min_moves(nodes: &HashMap<Point, Node>) -> i32 {
    let (width, _) = dimensions(nodes);
    let goal = Point::new(width, 0);
    let hole = find_hole(nodes).expect("No hole found");

    let mut sum = 0;
    let mut hole = hole;
    let mut goal = goal;

    while goal != Point::new(0, 0) {
        let next = Point::new(goal.x - 1, goal.y);
        let m = moves(nodes, goal, hole, next).expect("No path");
        sum += m;
        hole = next;
        let m = moves(nodes, goal, goal, hole).expect("No path");
        sum += m;
        std::mem::swap(&mut goal, &mut hole);
    }
    sum
}

fn find_hole(nodes: &HashMap<Point, Node>) -> Option<Point> {
    nodes.iter().find_map(|(&p, n)| if n.used == 0 { Some(p) } else { None })
}

fn moves(
    nodes: &HashMap<Point, Node>,
    goal: Point,
    from: Point,
    to: Point,
) -> Result<i32, &'static str> {
    let (width, height) = dimensions(nodes);
    let mut distances = HashMap::new();
    let mut heap = BinaryHeap::new();

    distances.insert(from, 0);
    heap.push(State { cost: 0, point: from });

    while let Some(State { cost, point }) = heap.pop() {
        if point == to {
            return Ok(cost);
        }

        for neighbor in point.neighbors() {
            if neighbor.x < 0 || neighbor.y < 0 || neighbor.x > width || neighbor.y > height {
                continue;
            }
            if let Some(node) = nodes.get(&neighbor) {
                if node.used > 400 || neighbor == goal {
                    continue;
                }
                let next_cost = cost + 1;
                if !distances.contains_key(&neighbor) || next_cost < *distances.get(&neighbor).unwrap()
                {
                    distances.insert(neighbor, next_cost);
                    heap.push(State {
                        cost: next_cost,
                        point: neighbor,
                    });
                }
            }
        }
    }

    Err("No possible path")
}

fn dimensions(nodes: &HashMap<Point, Node>) -> (i32, i32) {
    nodes.keys().fold((0, 0), |(w, h), p| {
        (w.max(p.x), h.max(p.y))
    })
}
