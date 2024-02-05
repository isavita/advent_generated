
use std::fs::File;
use std::io::{BufReader, prelude::*};
use std::collections::HashMap;

fn main() {
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut heightmap: Vec<Vec<i32>> = Vec::new();
    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        let row: Vec<i32> = line.chars().map(|c| c.to_digit(10).unwrap() as i32).collect();
        heightmap.push(row);
    }

    let mut basin_sizes: Vec<i32> = Vec::new();
    let mut visited: HashMap<(i32, i32), bool> = HashMap::new();

    for (y, row) in heightmap.iter().enumerate() {
        for (x, _) in row.iter().enumerate() {
            if is_low_point(&heightmap, x, y) {
                let size = explore_basin(&heightmap, x as i32, y as i32, &mut visited);
                basin_sizes.push(size);
            }
        }
    }

    basin_sizes.sort_by(|a, b| b.cmp(a));
    let result = basin_sizes[0] * basin_sizes[1] * basin_sizes[2];
    println!("{}", result);
}

fn is_low_point(heightmap: &Vec<Vec<i32>>, x: usize, y: usize) -> bool {
    let height = heightmap[y][x];
    if x > 0 && heightmap[y][x-1] <= height {
        return false;
    }
    if x < heightmap[y].len() - 1 && heightmap[y][x+1] <= height {
        return false;
    }
    if y > 0 && heightmap[y-1][x] <= height {
        return false;
    }
    if y < heightmap.len() - 1 && heightmap[y+1][x] <= height {
        return false;
    }
    true
}

fn explore_basin(heightmap: &Vec<Vec<i32>>, x: i32, y: i32, visited: &mut HashMap<(i32, i32), bool>) -> i32 {
    if *visited.get(&(x, y)).unwrap_or(&false) || heightmap[y as usize][x as usize] == 9 {
        return 0;
    }
    visited.insert((x, y), true);
    let mut size = 1;

    let directions = vec![(0, -1), (-1, 0), (0, 1), (1, 0)];
    for dir in directions {
        let new_x = x + dir.0;
        let new_y = y + dir.1;
        if new_x >= 0 && new_x < heightmap[0].len() as i32 && new_y >= 0 && new_y < heightmap.len() as i32 {
            size += explore_basin(heightmap, new_x, new_y, visited);
        }
    }
    size
}
