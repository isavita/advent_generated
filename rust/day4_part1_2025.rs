
use std::fs;

fn main() {
    let txt = fs::read_to_string("input.txt").unwrap_or_default();
    let grid: Vec<Vec<char>> = txt.lines()
        .filter(|l| !l.is_empty())
        .map(|l| l.chars().collect())
        .collect();
    if grid.is_empty() {
        println!("Empty grid");
        return;
    }
    let (rows, cols) = (grid.len(), grid[0].len());
    let mut count = 0;
    for y in 0..rows {
        for x in 0..cols {
            if grid[y][x] != '@' { continue; }
            let mut n = 0;
            for dy in -1..=1 {
                for dx in -1..=1 {
                    if dx == 0 && dy == 0 { continue; }
                    let ny = y as isize + dy;
                    let nx = x as isize + dx;
                    if ny >= 0 && (ny as usize) < rows &&
                       nx >= 0 && (nx as usize) < cols &&
                       grid[ny as usize][nx as usize] == '@' {
                        n += 1;
                    }
                }
            }
            if n < 4 { count += 1; }
        }
    }
    println!("Number of accessible rolls of paper: {}", count);
}
