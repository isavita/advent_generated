
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let grid: Vec<Vec<char>> = io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().chars().collect())
        .collect();

    let h = grid.len();
    let w = grid[0].len();
    let mut x = 0;
    let mut y = 0;
    let mut dir_x = 0;
    let mut dir_y = 0;
    let dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
    let mut dir_idx = 0;
    let mut found = false;

    for i in 0..h {
        for j in 0..w {
            match grid[i][j] {
                '^' => {
                    x = j as i32;
                    y = i as i32;
                    dir_idx = 0;
                    dir_x = dirs[dir_idx][0];
                    dir_y = dirs[dir_idx][1];
                    found = true;
                    break;
                },
                '>' => {
                     x = j as i32;
                    y = i as i32;
                    dir_idx = 1;
                    dir_x = dirs[dir_idx][0];
                    dir_y = dirs[dir_idx][1];
                    found = true;
                     break;
                },
                'v' => {
                    x = j as i32;
                    y = i as i32;
                    dir_idx = 2;
                    dir_x = dirs[dir_idx][0];
                    dir_y = dirs[dir_idx][1];
                    found = true;
                     break;

                },
                '<' => {
                     x = j as i32;
                    y = i as i32;
                    dir_idx = 3;
                    dir_x = dirs[dir_idx][0];
                    dir_y = dirs[dir_idx][1];
                    found = true;
                     break;
                },
                _ => {}
            }
        }
         if found {
            break;
        }
    }

    let mut visited = HashSet::new();
    visited.insert((x, y));

    loop {
        let nx = x + dir_x;
        let ny = y + dir_y;
        if nx < 0 || nx >= w as i32 || ny < 0 || ny >= h as i32 {
            break;
        }
        if grid[ny as usize][nx as usize] == '#' {
            dir_idx = (dir_idx + 1) % 4;
            dir_x = dirs[dir_idx][0];
            dir_y = dirs[dir_idx][1];
            continue;
        }
        x = nx;
        y = ny;
        visited.insert((x, y));
    }

    println!("{}", visited.len());
    Ok(())
}
