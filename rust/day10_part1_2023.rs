
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Tile {
    Vertical,
    Horizontal,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Ground,
    Start,
}

impl Tile {
    fn from_char(c: char) -> Self {
        match c {
            '|' => Tile::Vertical,
            '-' => Tile::Horizontal,
            'L' => Tile::NorthEast,
            'J' => Tile::NorthWest,
            '7' => Tile::SouthWest,
            'F' => Tile::SouthEast,
            '.' => Tile::Ground,
            'S' => Tile::Start,
            _ => panic!("Invalid tile character: {}", c),
        }
    }
}

fn get_input(filename: impl AsRef<Path>) -> io::Result<Vec<Vec<Tile>>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);

    let mut grid = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let row = line.chars().map(Tile::from_char).collect();
        grid.push(row);
    }
    Ok(grid)
}

fn find_start(grid: &Vec<Vec<Tile>>) -> (usize, usize) {
    for (r, row) in grid.iter().enumerate() {
        for (c, &tile) in row.iter().enumerate() {
            if tile == Tile::Start {
                return (r, c);
            }
        }
    }
    panic!("Start tile not found");
}

fn get_possible_moves(
    grid: &Vec<Vec<Tile>>,
    row: usize,
    col: usize,
) -> Vec<(usize, usize)> {
    let mut moves = Vec::new();
    let current_tile = &grid[row][col];
    let max_rows = grid.len();
    let max_cols = grid[0].len();
    match current_tile {
        Tile::Vertical => {
            if row > 0 { moves.push((row - 1, col)) }
            if row < max_rows - 1 { moves.push((row + 1, col)) }
        }
        Tile::Horizontal => {
             if col > 0 { moves.push((row, col - 1)) }
             if col < max_cols - 1 { moves.push((row, col + 1)) }
        }
        Tile::NorthEast => {
            if row > 0 { moves.push((row - 1, col)) }
            if col < max_cols - 1 { moves.push((row, col + 1)) }
        }
        Tile::NorthWest => {
            if row > 0 { moves.push((row - 1, col)) }
            if col > 0 { moves.push((row, col - 1)) }
        }
        Tile::SouthWest => {
            if row < max_rows - 1 { moves.push((row + 1, col)) }
             if col > 0 { moves.push((row, col - 1)) }
        }
        Tile::SouthEast => {
            if row < max_rows - 1 { moves.push((row + 1, col)) }
            if col < max_cols - 1 { moves.push((row, col + 1)) }
        }
        Tile::Start => {
             if row > 0 && [Tile::Vertical, Tile::SouthEast, Tile::SouthWest].contains(&grid[row-1][col]){ moves.push((row - 1, col)) }
            if row < max_rows - 1 && [Tile::Vertical, Tile::NorthEast, Tile::NorthWest].contains(&grid[row+1][col]) { moves.push((row + 1, col)) }
            if col > 0 && [Tile::Horizontal, Tile::NorthEast, Tile::SouthEast].contains(&grid[row][col-1]) { moves.push((row, col - 1)) }
            if col < max_cols - 1 && [Tile::Horizontal, Tile::NorthWest, Tile::SouthWest].contains(&grid[row][col+1]) { moves.push((row, col + 1)) }
        }
        Tile::Ground => {}
    }
    moves
}

fn solve() -> io::Result<usize> {
    let grid = get_input("input.txt")?;
    let (start_row, start_col) = find_start(&grid);

    let mut max_distance = 0;
    let mut visited = vec![vec![false; grid[0].len()]; grid.len()];
    
    let mut queue = Vec::new();
    queue.push((start_row, start_col, 0));
    visited[start_row][start_col] = true;
    
    while !queue.is_empty() {
        let (row, col, dist) = queue.remove(0);
        max_distance = std::cmp::max(max_distance, dist);

        for (next_row, next_col) in get_possible_moves(&grid, row, col) {
            if !visited[next_row][next_col] {
                visited[next_row][next_col] = true;
                 queue.push((next_row, next_col, dist + 1));
            }
        }
    }

    Ok(max_distance)
}

fn main() -> io::Result<()> {
    let result = solve()?;
    println!("{}", result);
    Ok(())
}
