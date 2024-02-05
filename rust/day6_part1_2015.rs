
use std::fs;

const GRID_SIZE: usize = 1000;

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Error reading file");
    let mut grid = vec![vec![false; GRID_SIZE]; GRID_SIZE];

    for instruction in contents.lines() {
        process_instruction(instruction, &mut grid);
    }

    println!("{}", count_lights(&grid));
}

fn process_instruction(instruction: &str, grid: &mut Vec<Vec<bool>>) {
    let parts: Vec<&str> = instruction.split_whitespace().collect();
    let (start_x, start_y) = scan_coords(parts[parts.len() - 3]);
    let (end_x, end_y) = scan_coords(parts[parts.len() - 1]);

    for x in start_x..=end_x {
        for y in start_y..=end_y {
            match parts[0] {
                "turn" => {
                    if parts[1] == "on" {
                        grid[x][y] = true;
                    } else {
                        grid[x][y] = false;
                    }
                }
                "toggle" => {
                    grid[x][y] = !grid[x][y];
                }
                _ => {}
            }
        }
    }
}

fn scan_coords(coord: &str) -> (usize, usize) {
    let coords: Vec<&str> = coord.split(',').collect();
    let x: usize = coords[0].parse().unwrap();
    let y: usize = coords[1].parse().unwrap();
    (x, y)
}

fn count_lights(grid: &Vec<Vec<bool>>) -> usize {
    grid.iter().flatten().filter(|&&light| light).count()
}
