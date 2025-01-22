
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::{VecDeque, HashSet};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct Point {
    x: i32,
    y: i32,
}

fn is_valid(p: &Point, grid_size: i32) -> bool {
    p.x >= 0 && p.x < grid_size && p.y >= 0 && p.y < grid_size
}

fn bfs(start: Point, end: Point, corrupted: &HashSet<Point>, grid_size: i32) -> Option<i32> {
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back((start, 0));
    visited.insert(start);

    while let Some((current, dist)) = queue.pop_front() {
        if current == end {
            return Some(dist);
        }

        let neighbors = [
            Point { x: current.x + 1, y: current.y },
            Point { x: current.x - 1, y: current.y },
            Point { x: current.x, y: current.y + 1 },
            Point { x: current.x, y: current.y - 1 },
        ];

        for neighbor in neighbors {
            if is_valid(&neighbor, grid_size) && !corrupted.contains(&neighbor) && !visited.contains(&neighbor) {
                queue.push_back((neighbor, dist + 1));
                visited.insert(neighbor);
            }
        }
    }

    None
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut bytes: Vec<(i32, i32)> = reader
        .lines()
        .filter_map(Result::ok)
        .map(|line| {
            let parts: Vec<&str> = line.split(',').collect();
            (parts[0].parse().unwrap(), parts[1].parse().unwrap())
        })
        .collect();

    let grid_size = 71;
    let start = Point { x: 0, y: 0 };
    let end = Point { x: grid_size - 1, y: grid_size - 1 };

    // Part 1
    let mut corrupted_part1 = HashSet::new();
    for i in 0..1024 {
        corrupted_part1.insert(Point { x: bytes[i].0, y: bytes[i].1 });
    }
    
    let steps_part1 = bfs(start, end, &corrupted_part1, grid_size).unwrap();
    println!("{}", steps_part1);
    
    // Part 2
    let mut corrupted_part2 = HashSet::new();
    let mut result_coords = (0,0);
     for i in 0..bytes.len() {
        corrupted_part2.insert(Point { x: bytes[i].0, y: bytes[i].1 });
        if bfs(start, end, &corrupted_part2, grid_size).is_none() {
            result_coords = bytes[i];
            break;
        }
    }

    println!("{},{}", result_coords.0, result_coords.1);

    Ok(())
}
