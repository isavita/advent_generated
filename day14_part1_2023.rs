
use std::fs;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    fn add(&self, other: Coord) -> Coord {
        Coord { x: self.x + other.x, y: self.y + other.y }
    }

    fn is_in_bounds(&self, grid: &Grid) -> bool {
        self.x >= 0 && self.x < grid.width && self.y >= 0 && self.y < grid.height
    }
}

struct Grid {
    width: i32,
    height: i32,
    data: std::collections::HashMap<Coord, u8>,
}

const EMPTY: u8 = b'.';
const CUBIC_ROCK: u8 = b'#';
const ROUND_ROCK: u8 = b'O';

fn build_grid(input: Vec<&str>) -> Grid {
    let mut data = std::collections::HashMap::new();
    let height = input.len() as i32;
    let width = input[0].len() as i32;

    for (y, line) in input.iter().enumerate() {
        for (x, &char) in line.as_bytes().iter().enumerate() {
            if char != EMPTY {
                data.insert(Coord { x: x as i32, y: y as i32 }, char);
            }
        }
    }

    Grid { width, height, data }
}

fn shift_single_rock(grid: &mut Grid, coord: Coord, dir: Coord) {
    if let Some(&ROUND_ROCK) = grid.data.get(&coord) {
        let mut current = coord;
        let mut before = coord.add(dir);

        while !grid.data.contains_key(&before) && before.is_in_bounds(grid) {
            grid.data.insert(before, ROUND_ROCK);
            grid.data.remove(&current);

            current = before;
            before = before.add(dir);
        }
    }
}

fn shift_rocks(grid: &mut Grid, dir: Coord) {
    match dir {
        Coord { x: 0, y: -1 } | Coord { x: -1, y: 0 } => {
            for x in 0..grid.width {
                for y in 0..grid.height {
                    shift_single_rock(grid, Coord { x, y }, dir);
                }
            }
        }
        Coord { x: 0, y: 1 } | Coord { x: 1, y: 0 } => {
            for x in (0..grid.width).rev() {
                for y in (0..grid.height).rev() {
                    shift_single_rock(grid, Coord { x, y }, dir);
                }
            }
        }
        _ => {}
    }
}

fn calculate_load(grid: &Grid) -> i32 {
    let mut load = 0;

    for x in 0..grid.width {
        for y in 0..grid.height {
            if let Some(&ROUND_ROCK) = grid.data.get(&Coord { x, y }) {
                load += grid.height - y;
            }
        }
    }

    load
}

fn solve(input: Vec<&str>) -> i32 {
    let mut grid = build_grid(input);
    shift_rocks(&mut grid, Coord { x: 0, y: -1 });

    calculate_load(&grid)
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let input_lines: Vec<&str> = input.trim().split('\n').collect();
    println!("{}", solve(input_lines));
}
