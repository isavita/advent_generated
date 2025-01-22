
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::VecDeque;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    const SIZE: usize = 71;
    let mut grid = [[false; SIZE]; SIZE];

    for (i, line) in reader.lines().enumerate() {
        if i >= 1024 {
            break;
        }
        let line = line?;
        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() == 2 {
            if let (Ok(x), Ok(y)) = (parts[0].parse::<usize>(), parts[1].parse::<usize>()) {
                if x < SIZE && y < SIZE {
                    grid[y][x] = true;
                }
            }
        }
    }

    let dirs: [(isize, isize); 4] = [(1, 0), (-1, 0), (0, 1), (0, -1)];
    let mut visited = [[false; SIZE]; SIZE];
    let mut q = VecDeque::new();
    q.push_back(((0, 0), 0));
    visited[0][0] = true;

    while let Some(((x, y), steps)) = q.pop_front() {
        if x == SIZE - 1 && y == SIZE - 1 {
            println!("{}", steps);
            return Ok(());
        }
        for &(dx, dy) in &dirs {
            let nx = x as isize + dx;
            let ny = y as isize + dy;
            if nx >= 0 && ny >= 0 && nx < SIZE as isize && ny < SIZE as isize {
                let nx = nx as usize;
                let ny = ny as usize;
                  if !grid[ny][nx] && !visited[ny][nx] {
                        visited[ny][nx] = true;
                        q.push_back(((nx, ny), steps + 1));
                }
            }
        }
    }

    println!("No path");
    Ok(())
}
