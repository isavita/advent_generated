use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

struct RebootStep {
    action: bool,
    x_start: i32,
    y_start: i32,
    z_start: i32,
    x_end: i32,
    y_end: i32,
    z_end: i32,
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut reboot_steps = Vec::new();

    for line in reader.lines() {
        let line = line?;
        if line.is_empty() {
            continue;
        }
        let step = parse_reboot_step(&line);
        reboot_steps.push(step);
    }

    let min_coord = -50;
    let max_coord = 50;
    let mut cube_grid = create_cube_grid(min_coord, max_coord);
    execute_reboot_steps(&mut cube_grid, &reboot_steps);
    let on_cubes = count_on_cubes(&cube_grid);

    println!("{}", on_cubes);

    Ok(())
}

fn parse_reboot_step(line: &str) -> RebootStep {
    let parts: Vec<_> = line.split_whitespace().collect();
    let action = parts[0] == "on";
    let ranges: Vec<_> = parts[1].split(',').collect();
    let x_range: Vec<_> = ranges[0][2..].split("..").collect();
    let y_range: Vec<_> = ranges[1][2..].split("..").collect();
    let z_range: Vec<_> = ranges[2][2..].split("..").collect();

    let x_start = x_range[0].parse().unwrap();
    let x_end = x_range[1].parse().unwrap();
    let y_start = y_range[0].parse().unwrap();
    let y_end = y_range[1].parse().unwrap();
    let z_start = z_range[0].parse().unwrap();
    let z_end = z_range[1].parse().unwrap();

    RebootStep {
        action,
        x_start,
        y_start,
        z_start,
        x_end,
        y_end,
        z_end,
    }
}

fn create_cube_grid(min_coord: i32, max_coord: i32) -> Vec<Vec<Vec<bool>>> {
    let grid_size = (max_coord - min_coord + 1) as usize;
    vec![vec![vec![false; grid_size]; grid_size]; grid_size]
}

fn execute_reboot_steps(cube_grid: &mut Vec<Vec<Vec<bool>>>, reboot_steps: &[RebootStep]) {
    for step in reboot_steps {
        if !(step.x_start >= -50 && step.x_end <= 50 && step.y_start >= -50 && step.y_end <= 50 && step.z_start >= -50 && step.z_end <= 50) {
            continue;
        }
        for x in step.x_start..=step.x_end {
            for y in step.y_start..=step.y_end {
                for z in step.z_start..=step.z_end {
                    let x_index = (x + 50) as usize;
                    let y_index = (y + 50) as usize;
                    let z_index = (z + 50) as usize;
                    if x_index < cube_grid.len() && y_index < cube_grid[0].len() && z_index < cube_grid[0][0].len() {
                        cube_grid[x_index][y_index][z_index] = step.action;
                    }
                }
            }
        }
    }
}

fn count_on_cubes(cube_grid: &Vec<Vec<Vec<bool>>>) -> i32 {
    let mut count = 0;
    for i in cube_grid {
        for j in i {
            for &k in j {
                if k {
                    count += 1;
                }
            }
        }
    }
    count
}