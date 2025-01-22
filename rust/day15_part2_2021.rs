
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let grid = read_grid_from_file("input.txt").expect("Failed to read input file");

    let part1_result = solve(&grid);
    println!("Part 1: {}", part1_result);

    let full_grid = expand_grid(&grid, 5);
    let part2_result = solve(&full_grid);
    println!("Part 2: {}", part2_result);
}

fn read_grid_from_file<P>(filename: P) -> io::Result<Vec<Vec<u32>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut grid = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let row: Vec<u32> = line
            .chars()
            .map(|c| c.to_digit(10).expect("Invalid input"))
            .collect();
        grid.push(row);
    }

    Ok(grid)
}

fn solve(grid: &Vec<Vec<u32>>) -> u32 {
    let rows = grid.len();
    let cols = grid[0].len();

    let start = (0, 0);
    let end = (rows - 1, cols - 1);

    let mut dist = vec![vec![u32::MAX; cols]; rows];
    dist[start.0][start.1] = 0;

    let mut queue = BinaryHeap::new();
    queue.push(Reverse((0, start)));

    while let Some(Reverse((d, (r, c)))) = queue.pop() {
        if (r, c) == end {
            return d;
        }

        if d > dist[r][c] {
            continue;
        }

        let moves = [(r as i32 + 1, c as i32), (r as i32 - 1, c as i32), (r as i32, c as i32 + 1), (r as i32, c as i32 -1)];

        for (nr, nc) in moves {
            if nr >= 0 && nr < rows as i32 && nc >= 0 && nc < cols as i32 {
                let nr = nr as usize;
                let nc = nc as usize;
                let new_dist = d + grid[nr][nc];
                if new_dist < dist[nr][nc] {
                    dist[nr][nc] = new_dist;
                    queue.push(Reverse((new_dist, (nr, nc))));
                }
            }
        }
    }
    
    panic!("No path found");
}

fn expand_grid(grid: &Vec<Vec<u32>>, factor: usize) -> Vec<Vec<u32>> {
    let rows = grid.len();
    let cols = grid[0].len();
    let new_rows = rows * factor;
    let new_cols = cols * factor;

    let mut expanded_grid = vec![vec![0; new_cols]; new_rows];

    for r in 0..new_rows {
        for c in 0..new_cols {
            let original_r = r % rows;
            let original_c = c % cols;
            let tile_r = r / rows;
            let tile_c = c / cols;

            let mut value = grid[original_r][original_c] + (tile_r + tile_c) as u32;
            while value > 9 {
                value -= 9;
            }
            expanded_grid[r][c] = value;
        }
    }
    expanded_grid
}
