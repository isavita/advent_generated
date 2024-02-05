
use std::collections::BinaryHeap;
use std::fs::File;
use std::io::{BufReader, prelude::*};
use std::cmp::Reverse;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn add(&self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut grid = HashMap::new();
    let mut start = Point { x: 0, y: 0 };
    let mut end = Point { x: 0, y: 0 };
    let mut as_points = vec![];
    let mut y = 0;

    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        for (x, b) in line.bytes().enumerate() {
            let p = Point { x: x as i32, y };
            grid.insert(p, b);
            match b {
                b'S' => start = p,
                b'E' => end = p,
                b'a' => as_points.push(p),
                _ => {}
            }
        }
        y += 1;
    }
    grid.insert(start, b'a');
    grid.insert(end, b'z');

    let dists = djikstra(&grid, end);

    let l = dists[&start];
    println!("{}", l);
}

#[derive(Debug, Eq, PartialEq)]
struct Item {
    obj: Point,
    priority: i32,
}

impl Ord for Item {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn djikstra(grid: &HashMap<Point, u8>, end: Point) -> HashMap<Point, i32> {
    let mut pq = BinaryHeap::new();
    pq.push(Item { obj: end, priority: 0 });
    let mut dist = HashMap::new();
    dist.insert(end, 0);

    while let Some(Item { obj: curr, priority: _ }) = pq.pop() {
        for n in &[Point { x: 0, y: 1 }, Point { x: 0, y: -1 }, Point { x: 1, y: 0 }, Point { x: -1, y: 0 }] {
            let next = curr.add(*n);
            if let Some(&next_val) = grid.get(&next) {
                if (grid[&curr] as i32) - (next_val as i32) > 1 {
                    continue;
                }
                let next_dist = dist[&curr] + 1;
                if !dist.contains_key(&next) || next_dist < dist[&next] {
                    dist.insert(next, next_dist);
                    pq.push(Item { obj: next, priority: next_dist });
                }
            }
        }
    }

    dist
}
