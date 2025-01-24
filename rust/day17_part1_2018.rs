
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Point {
    x: usize,
    y: usize,
}

fn parse_input(filename: &str) -> (HashSet<Point>, usize, usize) {
    let file = File::open(filename).expect("Failed to open file");
    let reader = BufReader::new(file);
    let mut clay = HashSet::new();
    let (mut min_y, mut max_y) = (usize::MAX, usize::MIN);

    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        let parts: Vec<&str> = line.split(", ").collect();
        let first_part = parts[0];
        let second_part = parts[1];

        if first_part.starts_with("x=") {
            let x: usize = first_part[2..].parse().unwrap();
            let y_range: Vec<usize> = second_part[2..]
                .split("..")
                .map(|s| s.parse().unwrap())
                .collect();
            for y in y_range[0]..=y_range[1] {
                clay.insert(Point { x, y });
                min_y = min_y.min(y);
                max_y = max_y.max(y);
            }
        } else {
            let y: usize = first_part[2..].parse().unwrap();
            let x_range: Vec<usize> = second_part[2..]
                .split("..")
                .map(|s| s.parse().unwrap())
                .collect();
            for x in x_range[0]..=x_range[1] {
                clay.insert(Point { x, y });
                min_y = min_y.min(y);
                max_y = max_y.max(y);
            }
        }
    }

    (clay, min_y, max_y)
}

fn flow(
    clay: &HashSet<Point>,
    wet: &mut HashSet<Point>,
    settled: &mut HashSet<Point>,
    min_y: usize,
    max_y: usize,
    start: Point,
) {
    if start.y > max_y || wet.contains(&start) {
        return;
    }

    wet.insert(start);

    let below = Point {
        x: start.x,
        y: start.y + 1,
    };
    if !clay.contains(&below) && !settled.contains(&below) {
        flow(clay, wet, settled, min_y, max_y, below);
    }

    if clay.contains(&below) || settled.contains(&below) {
        let mut left = Point {
            x: start.x - 1,
            y: start.y,
        };
        let mut right = Point {
            x: start.x + 1,
            y: start.y,
        };
        let mut can_settle = true;

        while !clay.contains(&left)
            && (clay.contains(&Point {
                x: left.x,
                y: left.y + 1,
            }) || settled.contains(&Point {
                x: left.x,
                y: left.y + 1,
            }))
        {
            wet.insert(left);
            left.x -= 1;
        }
        if !clay.contains(&left) {
            can_settle = false;
            flow(clay, wet, settled, min_y, max_y, left);
        }

        while !clay.contains(&right)
            && (clay.contains(&Point {
                x: right.x,
                y: right.y + 1,
            }) || settled.contains(&Point {
                x: right.x,
                y: right.y + 1,
            }))
        {
            wet.insert(right);
            right.x += 1;
        }
        if !clay.contains(&right) {
            can_settle = false;
            flow(clay, wet, settled, min_y, max_y, right);
        }

        if can_settle {
            for x in left.x + 1..right.x {
                settled.insert(Point { x, y: start.y });
            }
        }
    }
}

fn main() {
    let (clay, min_y, max_y) = parse_input("input.txt");
    let mut wet = HashSet::new();
    let mut settled = HashSet::new();
    flow(
        &clay,
        &mut wet,
        &mut settled,
        min_y,
        max_y,
        Point { x: 500, y: 0 },
    );

    let count = wet
        .iter()
        .filter(|p| p.y >= min_y && p.y <= max_y)
        .count();
    println!("{}", count);
}
