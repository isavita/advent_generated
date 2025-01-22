
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .filter_map(Result::ok)
        .collect();

    let rows = lines.len();
    let cols = if rows > 0 { lines[0].len() } else { 0 };
    let grid: Vec<Vec<char>> = lines.iter().map(|line| line.chars().collect()).collect();

    let mut count = 0;
    let target = ['X', 'M', 'A', 'S'];

    for row in 0..rows {
        for col in 0..cols {
            // Check horizontal (forward)
            if col + 3 < cols && grid[row][col..=col + 3] == target {
                count += 1;
            }
            // Check horizontal (backward)
             if col >= 3 && (0..4).all(|i| grid[row][col-i] == target[i]) {
                count +=1;
            }
             // Check vertical (forward)
            if row + 3 < rows && (0..4).all(|i| grid[row+i][col] == target[i]) {
              count += 1;
           }
           // Check vertical (backward)
           if row >= 3 && (0..4).all(|i| grid[row-i][col] == target[i]) {
              count += 1;
           }
             // Check diagonal (top-left to bottom-right)
            if row + 3 < rows && col + 3 < cols && (0..4).all(|i| grid[row+i][col+i] == target[i]) {
                count += 1;
            }
           // Check diagonal (bottom-right to top-left)
            if row >= 3 && col >= 3 && (0..4).all(|i| grid[row-i][col-i] == target[i]) {
                count += 1;
            }
             // Check diagonal (top-right to bottom-left)
            if row + 3 < rows && col >= 3 && (0..4).all(|i| grid[row + i][col - i] == target[i]) {
                count += 1;
            }
            // Check diagonal (bottom-left to top-right)
            if row >= 3 && col + 3 < cols && (0..4).all(|i| grid[row - i][col + i] == target[i]) {
                count += 1;
            }
        }
    }

    println!("{}", count);

    Ok(())
}
