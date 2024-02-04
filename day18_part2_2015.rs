use std::fs;

const GRID_SIZE: usize = 100;
const STEPS: usize = 100;

fn count_on_neighbors(grid: &Vec<Vec<bool>>, x: usize, y: usize) -> usize {
    let mut on = 0;
    for dx in -1..=1 {
        for dy in -1..=1 {
            if dx == 0 && dy == 0 {
                continue;
            }
            let nx = x as i32 + dx;
            let ny = y as i32 + dy;
            if nx >= 0 && nx < GRID_SIZE as i32 && ny >= 0 && ny < GRID_SIZE as i32 && grid[nx as usize][ny as usize] {
                on += 1;
            }
        }
    }
    on
}

fn step(grid: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    let mut new_grid = vec![vec![false; GRID_SIZE]; GRID_SIZE];

    for x in 0..GRID_SIZE {
        for y in 0..GRID_SIZE {
            let on_neighbors = count_on_neighbors(grid, x, y);
            if grid[x][y] {
                new_grid[x][y] = on_neighbors == 2 || on_neighbors == 3;
            } else {
                new_grid[x][y] = on_neighbors == 3;
            }
        }
    }

    // Ensure corners are always on
    new_grid[0][0] = true;
    new_grid[0][GRID_SIZE - 1] = true;
    new_grid[GRID_SIZE - 1][0] = true;
    new_grid[GRID_SIZE - 1][GRID_SIZE - 1] = true;

    new_grid
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Error reading file");
    let mut grid = vec![vec![false; GRID_SIZE]; GRID_SIZE];

    let mut y = 0;
    for line in input.lines() {
        for (x, c) in line.chars().enumerate() {
            grid[x][y] = c == '#';
        }
        y += 1;
    }

    // Initialize corners as always on
    grid[0][0] = true;
    grid[0][GRID_SIZE - 1] = true;
    grid[GRID_SIZE - 1][0] = true;
    grid[GRID_SIZE - 1][GRID_SIZE - 1] = true;

    for _ in 0..STEPS {
        grid = step(&grid);
    }

    let mut on_count = 0;
    for row in &grid {
        for &light in row {
            if light {
                on_count += 1;
            }
        }
    }

    println!("{}", on_count);
}