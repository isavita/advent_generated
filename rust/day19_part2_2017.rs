
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let grid: Vec<Vec<u8>> = io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().bytes().collect())
        .collect();

    let mut x = grid[0]
        .iter()
        .position(|&c| c == b'|')
        .expect("Start not found") as i32;
    let mut y = 0;
    let mut dx = 0;
    let mut dy = 1;
    let mut steps = 0;

    loop {
        if x < 0 || x >= grid[0].len() as i32 || y < 0 || y >= grid.len() as i32 {
            break;
        }
        let cell = grid[y as usize][x as usize];

        if cell == b' ' {
            break;
        }
        steps += 1;


        if cell == b'+' {
            if dx == 0 {
                if x > 0 && (grid[y as usize][(x - 1) as usize] == b'-' || (grid[y as usize][(x - 1) as usize] >= b'A' && grid[y as usize][(x - 1) as usize] <= b'Z')) {
                    dx = -1;
                    dy = 0;
                } else {
                    dx = 1;
                    dy = 0;
                }
            } else {
                if y > 0 && (grid[(y - 1) as usize][x as usize] == b'|' || (grid[(y - 1) as usize][x as usize] >= b'A' && grid[(y - 1) as usize][x as usize] <= b'Z')) {
                    dx = 0;
                    dy = -1;
                } else {
                    dx = 0;
                    dy = 1;
                }
            }
        }
        x += dx;
        y += dy;
    }
    println!("{}", steps);
    Ok(())
}
