
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(PartialEq, Eq, Clone, Copy)]
enum Tile {
    Sand,
    Clay,
    Water,
    WetSand,
}

fn parse_input(filename: &str) -> (HashMap<(i32, i32), Tile>, i32, i32) {
    let file = File::open(filename).expect("Failed to open input file");
    let reader = BufReader::new(file);
    let mut grid: HashMap<(i32, i32), Tile> = HashMap::new();
    let mut min_y = i32::MAX;
    let mut max_y = i32::MIN;

    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        let parts: Vec<&str> = line.split(", ").collect();
        let first_coord = parts[0];
        let second_coord = parts[1];

        let (first_type, first_val_str) = first_coord.split_at(2);
        let first_val = first_val_str.parse::<i32>().unwrap();

        let (second_type, second_range_str) = second_coord.split_at(2);
        let second_range_parts: Vec<&str> = second_range_str.split("..").collect();
        let second_start = second_range_parts[0].parse::<i32>().unwrap();
        let second_end = second_range_parts[1].parse::<i32>().unwrap();

        for second_val in second_start..=second_end {
            if first_type == "x=" {
                grid.insert((first_val, second_val), Tile::Clay);
                min_y = min_y.min(second_val);
                max_y = max_y.max(second_val);
            } else {
                grid.insert((second_val, first_val), Tile::Clay);
                min_y = min_y.min(first_val);
                max_y = max_y.max(first_val);
            }
        }
    }
    (grid, min_y, max_y)
}

fn flow(
    grid: &mut HashMap<(i32, i32), Tile>,
    x: i32,
    y: i32,
    min_y: i32,
    max_y: i32,
) -> bool {
    if y > max_y {
        return false;
    }

    match grid.get(&(x, y)) {
        Some(Tile::Clay) | Some(Tile::Water) => return true,
        Some(Tile::WetSand) => return false,
        None | Some(Tile::Sand) => {}
    }

    grid.insert((x, y), Tile::WetSand);

    if !flow(grid, x, y + 1, min_y, max_y) {
        return false;
    }
    let left_blocked = flow_sideways(grid, x - 1, y, min_y, max_y, -1);
    let right_blocked = flow_sideways(grid, x + 1, y, min_y, max_y, 1);

    if left_blocked && right_blocked {
        grid.insert((x, y), Tile::Water);
        fill_row(grid, x - 1, y, -1);
        fill_row(grid, x + 1, y, 1);

    }
    left_blocked && right_blocked

}

fn flow_sideways(
    grid: &mut HashMap<(i32, i32), Tile>,
    x: i32,
    y: i32,
    min_y: i32,
    max_y: i32,
    dx: i32,
) -> bool {
    match grid.get(&(x, y)) {
        Some(Tile::Clay) => return true,
        Some(Tile::Water) => return true,
        Some(Tile::WetSand) => {}
        None | Some(Tile::Sand) => {}
    }

    grid.insert((x, y), Tile::WetSand);

    if let Some(Tile::Sand) | None = grid.get(&(x, y + 1)) {
        if flow(grid, x, y + 1, min_y, max_y) {
            return flow_sideways(grid, x + dx, y, min_y, max_y, dx);
        } else {
            return false
        }
    } else {
        return flow_sideways(grid, x + dx, y, min_y, max_y, dx)
    }
}

fn fill_row(grid: &mut HashMap<(i32, i32), Tile>, x: i32, y: i32, dx: i32) {
    match grid.get(&(x,y)) {
        Some(Tile::WetSand) => {
            grid.insert((x,y), Tile::Water);
            fill_row(grid, x + dx, y, dx);
        }
        _ => {}
    }
}

fn solve(filename: &str) -> (usize, usize) {
    let (mut grid, min_y, max_y) = parse_input(filename);
    flow(&mut grid, 500, 0, min_y, max_y);

    let reachable_count = grid
        .iter()
        .filter(|(&(.., y), &tile)| y >= min_y && y <= max_y && (tile == Tile::Water || tile == Tile::WetSand))
        .count();

    let retained_count = grid
        .iter()
        .filter(|(&(.., y), &tile)| y >= min_y && y <= max_y && tile == Tile::Water)
        .count();
        
    (reachable_count, retained_count)
}
fn main() {
    let (part1, part2) = solve("input.txt");
    println!("{}", part1);
    println!("{}", part2);
}
