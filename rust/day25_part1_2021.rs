
use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut grid: Vec<Vec<char>> = contents.lines().map(|line| line.chars().collect()).collect();

    let rows = grid.len();
    let cols = grid[0].len();

    let mut steps = 0;
    loop {
        steps += 1;
        let mut moved = false;
        let mut new_grid = grid.clone();

        // Move east-facing cucumbers
        for r in 0..rows {
            for c in 0..cols {
                if grid[r][c] == '>' {
                    let next_c = (c + 1) % cols;
                    if grid[r][next_c] == '.' {
                        new_grid[r][c] = '.';
                        new_grid[r][next_c] = '>';
                        moved = true;
                    }
                }
            }
        }
        grid = new_grid.clone();
        
        // Move south-facing cucumbers
        for r in 0..rows {
            for c in 0..cols {
                if grid[r][c] == 'v' {
                    let next_r = (r + 1) % rows;
                    if grid[next_r][c] == '.' {
                        new_grid[r][c] = '.';
                        new_grid[next_r][c] = 'v';
                         moved = true;
                    }
                }
            }
        }
       
        grid = new_grid;

        if !moved {
            println!("{}", steps);
            break;
        }
    }
}
