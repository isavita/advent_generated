
use std::fs::File;
use std::io::{BufReader, prelude::*};

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut heightmap: Vec<Vec<i32>> = Vec::new();
    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        let mut row: Vec<i32> = Vec::new();
        for c in line.chars() {
            let height = c.to_digit(10).unwrap() as i32;
            row.push(height);
        }
        heightmap.push(row);
    }

    let mut total_risk_level = 0;
    for (y, row) in heightmap.iter().enumerate() {
        for (x, &height) in row.iter().enumerate() {
            if is_low_point(&heightmap, x, y) {
                total_risk_level += 1 + height;
            }
        }
    }

    println!("{}", total_risk_level);
}

fn is_low_point(heightmap: &Vec<Vec<i32>>, x: usize, y: usize) -> bool {
    let height = heightmap[y][x];
    if x > 0 && heightmap[y][x - 1] <= height {
        return false;
    }
    if x < heightmap[y].len() - 1 && heightmap[y][x + 1] <= height {
        return false;
    }
    if y > 0 && heightmap[y - 1][x] <= height {
        return false;
    }
    if y < heightmap.len() - 1 && heightmap[y + 1][x] <= height {
        return false;
    }
    true
}
