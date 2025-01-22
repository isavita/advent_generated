
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let grid: Vec<Vec<char>> = io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().chars().collect())
        .collect();

    let mut x = 0;
    for i in 0..grid[0].len() {
        if grid[0][i] == '|' {
            x = i;
            break;
        }
    }

    let (mut dx, mut dy) = (0, 1);
    let mut letters = String::new();
    let (rows, cols) = (grid.len(), grid[0].len());

    let mut y = 0;
    while y < rows && x < cols {
        let cell = grid[y][x];

         if cell == ' ' {
            break;
        }

        if cell.is_ascii_uppercase() {
            letters.push(cell);
        }

        if cell == '+' {
            if dx == 0 {
               if x > 0 && (grid[y][x-1] == '-' || grid[y][x-1].is_ascii_uppercase()) {
                    dx = -1;
                    dy = 0;
                } else {
                   dx = 1;
                    dy = 0;
                }
            } else {
               if y > 0 && (grid[y - 1][x] == '|' || grid[y - 1][x].is_ascii_uppercase()) {
                   dx = 0;
                    dy = -1;
                } else {
                   dx = 0;
                    dy = 1;
               }
            }
        }
         x = (x as isize + dx) as usize;
        y = (y as isize + dy) as usize;
    }

    println!("{}", letters);
    Ok(())
}
