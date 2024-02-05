
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
struct Position {
    x: i32,
    y: i32,
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(&path)?;
    let lines = io::BufReader::new(file).lines();

    let mut grid: HashMap<Position, bool> = HashMap::new();
    let (mut start_x, mut start_y) = (0, 0);
    for (y, line) in lines.enumerate() {
        let line = line?;
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                grid.insert(Position { x: x as i32, y: y as i32 }, true);
            }
        }
        start_x = line.len() as i32 / 2;
        start_y = y as i32 / 2;
    }

    // Directions: up, right, down, left
    let dx = [0, 1, 0, -1];
    let dy = [-1, 0, 1, 0];

    let (mut x, mut y, mut dir) = (start_x, start_y, 0);
    let mut infected_count = 0;

    for _ in 0..10000 {
        let pos = Position { x, y };
        if *grid.get(&pos).unwrap_or(&false) {
            dir = (dir + 1) % 4; // Turn right
            grid.remove(&pos); // Clean
        } else {
            dir = (dir + 3) % 4; // Turn left (equivalent to dir - 1 + 4)
            grid.insert(pos, true); // Infect
            infected_count += 1;
        }
        x += dx[dir];
        y += dy[dir];
    }

    println!("{}", infected_count);

    Ok(())
}
