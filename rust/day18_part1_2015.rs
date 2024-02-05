
use std::fs;

fn count_neighbors(grid: &Vec<Vec<char>>, x: usize, y: usize) -> usize {
    let mut count = 0;
    let directions = vec![-1, 0, 1];
    
    for &i in directions.iter() {
        for &j in directions.iter() {
            if i == 0 && j == 0 {
                continue;
            }
            let new_x = x as i32 + i;
            let new_y = y as i32 + j;
            if new_x >= 0 && new_x < grid.len() as i32 && new_y >= 0 && new_y < grid[0].len() as i32 {
                if grid[new_x as usize][new_y as usize] == '#' {
                    count += 1;
                }
            }
        }
    }
    
    count
}

fn next_state(grid: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut new_grid = grid.clone();
    
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            let neighbors = count_neighbors(&grid, i, j);
            if grid[i][j] == '#' {
                if neighbors != 2 && neighbors != 3 {
                    new_grid[i][j] = '.';
                }
            } else {
                if neighbors == 3 {
                    new_grid[i][j] = '#';
                }
            }
        }
    }
    
    new_grid
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    
    for _ in 0..100 {
        grid = next_state(&grid);
    }
    
    let lights_on = grid.iter().map(|row| row.iter().filter(|&&c| c == '#').count()).sum::<usize>();
    
    println!("{}", lights_on);
}

