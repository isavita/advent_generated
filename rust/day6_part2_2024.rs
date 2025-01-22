
use std::fs::File;
use std::io::{self, BufRead};

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct State {
    x: usize,
    y: usize,
    dir: usize,
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let grid: Vec<Vec<char>> = io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().chars().collect())
        .collect();

    let h = grid.len();
    let w = grid[0].len();

    let mut start_x = 0;
    let mut start_y = 0;
    let mut start_dir = 0;

    for (i, row) in grid.iter().enumerate() {
        for (j, &c) in row.iter().enumerate() {
            match c {
                '^' => {
                    start_x = j;
                    start_y = i;
                    start_dir = 0;
                }
                '>' => {
                    start_x = j;
                    start_y = i;
                    start_dir = 1;
                }
                'v' => {
                    start_x = j;
                    start_y = i;
                    start_dir = 2;
                }
                '<' => {
                    start_x = j;
                    start_y = i;
                    start_dir = 3;
                }
                _ => {}
            }
        }
    }
    
    let mut grid_copy = grid.clone();
    grid_copy[start_y][start_x] = '.';


    let mut can_loop = 0;
    for y in 0..h {
        for x in 0..w {
            if x == start_x && y == start_y {
                continue;
            }
            if grid_copy[y][x] != '.' {
                continue;
            }
           
            let mut temp_grid = grid_copy.clone();
             temp_grid[y][x] = '#';
            if loops(&temp_grid, start_x, start_y, start_dir) {
                can_loop += 1;
            }
        }
    }

    println!("{}", can_loop);
    Ok(())
}

fn loops(grid: &Vec<Vec<char>>, sx: usize, sy: usize, sdir: usize) -> bool {
    let h = grid.len();
    let w = grid[0].len();
    let dirs: [[isize; 2]; 4] = [[0, -1], [1, 0], [0, 1], [-1, 0]];
    let mut x = sx;
    let mut y = sy;
    let mut dir = sdir;
    let mut seen = std::collections::HashSet::new();

    for _ in 0..2000000 {
        let st = State { x, y, dir };
        if seen.contains(&st) {
            return true;
        }
        seen.insert(st);

        let dx = dirs[dir][0];
        let dy = dirs[dir][1];
        let nx = (x as isize + dx) as usize;
        let ny = (y as isize + dy) as usize;

        if nx >= w || ny >= h {
            return false;
        }
        if grid[ny][nx] == '#' {
            dir = (dir + 1) % 4;
            continue;
        }
        x = nx;
        y = ny;
    }
    false
}
