
use std::fs::File;
use std::io::{BufReader, prelude::*};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);

    let mut head = Point { x: 0, y: 0 };
    let mut tail = Point { x: 0, y: 0 };
    let mut visited: HashMap<Point, bool> = HashMap::new();
    visited.insert(tail, true);

    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split(" ").collect();
        let dir = parts[0];
        let steps: i32 = parts[1].parse().unwrap();

        for _ in 0..steps {
            match dir {
                "R" => head.x += 1,
                "L" => head.x -= 1,
                "U" => head.y += 1,
                "D" => head.y -= 1,
                _ => {}
            }

            if (head.x - tail.x).abs() > 1 || (head.y - tail.y).abs() > 1 {
                if head.x != tail.x && head.y != tail.y {
                    if head.x > tail.x {
                        tail.x += 1;
                    } else {
                        tail.x -= 1;
                    }
                    if head.y > tail.y {
                        tail.y += 1;
                    } else {
                        tail.y -= 1;
                    }
                } else {
                    if head.x > tail.x {
                        tail.x += 1;
                    } else if head.x < tail.x {
                        tail.x -= 1;
                    }
                    if head.y > tail.y {
                        tail.y += 1;
                    } else if head.y < tail.y {
                        tail.y -= 1;
                    }
                }
            }

            visited.insert(tail, true);
        }
    }

    println!("{}", visited.len());
}
