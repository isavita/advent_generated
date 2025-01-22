
use std::fs::File;
use std::io::{self, BufRead};

fn check_mas(grid: &[String], x: i32, y: i32, dx: i32, dy: i32) -> bool {
    let rows = grid.len() as i32;
    let cols = if rows > 0 { grid[0].len() as i32 } else { 0 };

    if x < 0 || y < 0 || x >= rows || y >= cols {
        return false;
    }

    let word = "MAS";
    let mut forward = true;
    let mut backward = true;

    for i in 0..word.len() {
        let new_x = x + (dx * i as i32);
        let new_y = y + (dy * i as i32);
        if new_x < 0 || new_y < 0 || new_x >= rows || new_y >= cols {
            forward = false;
            break;
        }
        if grid[new_x as usize].as_bytes()[new_y as usize] != word.as_bytes()[i] {
            forward = false;
        }
    }

      for i in 0..word.len() {
          let new_x = x + (dx * i as i32);
          let new_y = y + (dy * i as i32);
          if new_x < 0 || new_y < 0 || new_x >= rows || new_y >= cols {
              backward = false;
              break;
          }
          if grid[new_x as usize].as_bytes()[new_y as usize] != word.as_bytes()[word.len() - 1 - i] {
              backward = false;
          }
    }

    forward || backward
}

fn check_xmas(grid: &[String], x: i32, y: i32) -> bool {
    if check_mas(grid, x - 1, y - 1, 1, 1) &&
        check_mas(grid, x - 1, y + 1, 1, -1) {
           return true;
        }

    if check_mas(grid, x + 1, y - 1, -1, 1) &&
        check_mas(grid, x + 1, y + 1, -1, -1) {
            return true;
        }
    false
}

fn count_xmas_patterns(grid: &[String]) -> usize {
    let rows = grid.len();
    if rows < 3 || (rows > 0 && grid[0].len() < 3) {
        return 0;
    }

    let mut count = 0;
    for i in 1..rows - 1 {
        let row_len = grid[i].len();
        for j in 1..row_len-1 {
           if grid[i].as_bytes()[j] == b'A' && check_xmas(grid, i as i32, j as i32){
                count += 1;
           }
        }
    }
    count
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let grid: Vec<String> = reader
        .lines()
        .filter_map(Result::ok)
        .filter(|line| !line.trim().is_empty())
        .collect();

    let count = count_xmas_patterns(&grid);
    println!("X-MAS patterns appear {} times in the word search", count);

    Ok(())
}
