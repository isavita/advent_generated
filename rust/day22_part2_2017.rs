
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
struct Position {
    x: i32,
    y: i32,
}

#[derive(Clone, Copy)]
enum State {
    Clean,
    Weakened,
    Infected,
    Flagged,
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(&path)?;
    let lines = io::BufReader::new(file).lines();

    let mut grid = HashMap::new();
    let (mut start_x, mut start_y) = (0, 0);

    for (y, line) in lines.enumerate() {
        let line = line?;
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                grid.insert(Position { x: x as i32, y: y as i32 }, State::Infected);
            }
        }
        start_x = line.len() as i32 / 2;
        start_y = y as i32 / 2;
    }

    let dx = [0, 1, 0, -1];
    let dy = [-1, 0, 1, 0];

    let (mut x, mut y, mut dir) = (start_x, start_y, 0);
    let mut infected_count = 0;

    for _ in 0..10000000 {
        let pos = Position { x, y };
        let state = *grid.get(&pos).unwrap_or(&State::Clean);
        match state {
            State::Clean => {
                dir = (dir - 1 + 4) % 4;
                grid.insert(pos, State::Weakened);
            }
            State::Weakened => {
                grid.insert(pos, State::Infected);
                infected_count += 1;
            }
            State::Infected => {
                dir = (dir + 1) % 4;
                grid.insert(pos, State::Flagged);
            }
            State::Flagged => {
                dir = (dir + 2) % 4;
                grid.remove(&pos);
            }
        }
        x += dx[dir as usize];
        y += dy[dir as usize];
    }

    println!("{}", infected_count);

    Ok(())
}
