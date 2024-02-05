
use std::fs::File;
use std::io::{BufReader, prelude::*};

#[derive(Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);

    let mut grid = std::collections::HashMap::new();

    for line in reader.lines() {
        let line = line.unwrap();
        let coords: Vec<&str> = line.split(" -> ").collect();
        let start_coords: Vec<&str> = coords[0].split(",").collect();
        let end_coords: Vec<&str> = coords[1].split(",").collect();

        let x1: i32 = start_coords[0].parse().unwrap();
        let y1: i32 = start_coords[1].parse().unwrap();
        let x2: i32 = end_coords[0].parse().unwrap();
        let y2: i32 = end_coords[1].parse().unwrap();

        if x1 == x2 {
            let (y_start, y_end) = if y1 > y2 { (y2, y1) } else { (y1, y2) };
            for y in y_start..=y_end {
                *grid.entry(Point { x: x1, y }).or_insert(0) += 1;
            }
        } else if y1 == y2 {
            let (x_start, x_end) = if x1 > x2 { (x2, x1) } else { (x1, x2) };
            for x in x_start..=x_end {
                *grid.entry(Point { x, y: y1 }).or_insert(0) += 1;
            }
        }
    }

    let mut overlap_count = 0;
    for &v in grid.values() {
        if v > 1 {
            overlap_count += 1;
        }
    }

    println!("{}", overlap_count);
}
