
use std::fs::File;
use std::io::{BufRead, BufReader};

const GRID_SIZE: usize = 1000;

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);

    let mut grid = vec![vec![0; GRID_SIZE]; GRID_SIZE];

    for line in reader.lines() {
        if let Ok(instruction) = line {
            process_instruction(&instruction, &mut grid);
        }
    }

    println!("{}", total_brightness(&grid));
}

fn process_instruction(instruction: &str, grid: &mut Vec<Vec<i32>>) {
    let parts: Vec<&str> = instruction.split_whitespace().collect();
    let (start_x, start_y) = scan_coordinates(parts[parts.len() - 3]);
    let (end_x, end_y) = scan_coordinates(parts[parts.len() - 1]);

    for x in start_x..=end_x {
        for y in start_y..=end_y {
            match parts[0] {
                "turn" => {
                    if parts[1] == "on" {
                        grid[x][y] += 1;
                    } else {
                        if grid[x][y] > 0 {
                            grid[x][y] -= 1;
                        }
                    }
                }
                "toggle" => {
                    grid[x][y] += 2;
                }
                _ => {}
            }
        }
    }
}

fn scan_coordinates(coord: &str) -> (usize, usize) {
    let coords: Vec<&str> = coord.split(',').collect();
    let x: usize = coords[0].parse().unwrap();
    let y: usize = coords[1].parse().unwrap();
    (x, y)
}

fn total_brightness(grid: &Vec<Vec<i32>>) -> i32 {
    let mut brightness = 0;
    for row in grid {
        for light in row {
            brightness += *light;
        }
    }
    brightness
}
