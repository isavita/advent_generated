use std::collections::{BinaryHeap, HashMap};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::cmp::Ordering;

type Point = (i32, i32);

#[derive(Eq, PartialEq)]
struct Item {
    obj: Point,
    priority: i32,
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Item) -> Option<Ordering> {
        Some(other.priority.cmp(&self.priority))
    }
}

impl Ord for Item {
    fn cmp(&self, other: &Item) -> Ordering {
        other.priority.cmp(&self.priority)
    }
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut grid: HashMap<Point, u8> = HashMap::new();
    let mut start: Point = (0, 0);
    let mut end: Point = (0, 0);
    let mut as_list: Vec<Point> = Vec::new();
    let mut y: i32 = 0;
    for line in reader.lines() {
        let line = line.unwrap();
        for (x, b) in line.bytes().enumerate() {
            let p = (x as i32, y);
            grid.insert(p, b);
            if b == b'S' {
                start = p;
            } else if b == b'E' {
                end = p;
            } else if b == b'a' {
                as_list.push(p);
            }
        }
        y += 1;
    }
    *grid.get_mut(&start).unwrap() = b'a';
    *grid.get_mut(&end).unwrap() = b'z';

    let dists = djikstra(grid.clone(), end);
    let mut l = *dists.get(&start).unwrap_or(&i32::MAX);
    for a in as_list {
        if let Some(d) = dists.get(&a) {
            l = l.min(*d);
        }
    }
    println!("{}", l);
}

fn djikstra(grid: HashMap<Point, u8>, end: Point) -> HashMap<Point, i32> {
    let mut pq: BinaryHeap<Item> = BinaryHeap::new();
    let mut dist: HashMap<Point, i32> = HashMap::new();
    pq.push(Item { obj: end, priority: 0 });
    dist.insert(end, 0);
    let neighbors = vec![(-1, 0), (1, 0), (0, -1), (0, 1)];
    while let Some(Item { obj: curr, .. }) = pq.pop() {
        for n in &neighbors {
            let next = (curr.0 + n.0, curr.1 + n.1);
            if let Some(b) = grid.get(&next) {
                if grid[&curr] as i32 - *b as i32 > 1 {
                    continue;
                }
                let nextdist = *dist.get(&curr).unwrap_or(&i32::MAX) + 1;
                if let Some(d) = dist.get(&next) {
                    if nextdist < *d {
                        dist.insert(next, nextdist);
                        pq.push(Item { obj: next, priority: nextdist });
                    }
                } else {
                    dist.insert(next, nextdist);
                    pq.push(Item { obj: next, priority: nextdist });
                }
            }
        }
    }
    dist
}