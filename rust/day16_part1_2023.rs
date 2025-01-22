
use std::{
    collections::{HashMap, HashSet},
    fs,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    fn add(&self, other: &Coord) -> Coord {
        Coord {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }

    fn rotate90(&self) -> Coord {
        Coord {
            x: self.y,
            y: -self.x,
        }
    }

    fn rotate_neg90(&self) -> Coord {
        Coord {
            x: -self.y,
            y: self.x,
        }
    }

    fn is_in_bounds(&self, grid: &Grid) -> bool {
        self.x >= 0 && self.x < grid.width && self.y >= 0 && self.y < grid.height
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Beam {
    origin: Coord,
    dir: Coord,
}

#[derive(Debug)]
struct Grid {
    width: i32,
    height: i32,
    data: HashMap<Coord, u8>,
}

const EMPTY: u8 = b'.';
const ASCENDING_MIRROR: u8 = b'/';
const DESCENDING_MIRROR: u8 = b'\\';
const VERTICAL_SPLITTER: u8 = b'|';
const HORIZONTAL_SPLITTER: u8 = b'-';

const NORTH: Coord = Coord { x: 0, y: -1 };
const WEST: Coord = Coord { x: -1, y: 0 };
const SOUTH: Coord = Coord { x: 0, y: 1 };
const EAST: Coord = Coord { x: 1, y: 0 };

fn build_grid(input: &str) -> Grid {
    let lines: Vec<&str> = input.lines().collect();
    let width = lines[0].len() as i32;
    let height = lines.len() as i32;
    let mut data = HashMap::new();

    for (y, line) in lines.iter().enumerate() {
        for (x, char) in line.bytes().enumerate() {
            if char != EMPTY {
                data.insert(Coord { x: x as i32, y: y as i32 }, char);
            }
        }
    }

    Grid {
        width,
        height,
        data,
    }
}

fn next_beam(grid: &Grid, beam: &Beam) -> Vec<Beam> {
    let mut beams = Vec::with_capacity(2);
    let char = grid.data.get(&beam.origin);

    if char.is_none() {
        let new_beam = Beam {
            origin: beam.origin.add(&beam.dir),
            dir: beam.dir,
        };
        beams.push(new_beam);
        return beams;
    }

    let char = char.unwrap();

    match char {
        &ASCENDING_MIRROR => {
            let new_dir = if beam.dir == NORTH || beam.dir == SOUTH {
                beam.dir.rotate_neg90()
            } else {
                beam.dir.rotate90()
            };

            let new_beam = Beam {
                origin: beam.origin.add(&new_dir),
                dir: new_dir,
            };
            beams.push(new_beam);
        }

        &DESCENDING_MIRROR => {
            let new_dir = if beam.dir == NORTH || beam.dir == SOUTH {
                beam.dir.rotate90()
            } else {
                beam.dir.rotate_neg90()
            };

            let new_beam = Beam {
                origin: beam.origin.add(&new_dir),
                dir: new_dir,
            };
             beams.push(new_beam);
        }

        &VERTICAL_SPLITTER if beam.dir == EAST || beam.dir == WEST => {
            let new_dir1 = beam.dir.rotate90();
            let new_beam1 = Beam {
                origin: beam.origin.add(&new_dir1),
                dir: new_dir1,
            };
            let new_dir2 = beam.dir.rotate_neg90();
            let new_beam2 = Beam {
                origin: beam.origin.add(&new_dir2),
                dir: new_dir2,
            };
            beams.push(new_beam1);
            beams.push(new_beam2);
        }

        &HORIZONTAL_SPLITTER if beam.dir == NORTH || beam.dir == SOUTH => {
            let new_dir1 = beam.dir.rotate90();
            let new_beam1 = Beam {
                origin: beam.origin.add(&new_dir1),
                dir: new_dir1,
            };
            let new_dir2 = beam.dir.rotate_neg90();
            let new_beam2 = Beam {
                origin: beam.origin.add(&new_dir2),
                dir: new_dir2,
            };
            beams.push(new_beam1);
            beams.push(new_beam2);
        }

        _ => {
            let new_beam = Beam {
                origin: beam.origin.add(&beam.dir),
                dir: beam.dir,
            };
            beams.push(new_beam);
        }
    }
    beams
}

fn calculate_propagation(grid: &Grid, start: Beam) -> HashSet<Beam> {
    let mut already_seen = HashSet::new();
    let mut to_explore = vec![start];

    while let Some(beam) = to_explore.pop() {
        if beam.origin.is_in_bounds(grid) && !already_seen.contains(&beam) {
            already_seen.insert(beam);
             to_explore.extend(next_beam(grid, &beam));
        }
    }

    already_seen
}

fn calculate_energization(already_seen: &HashSet<Beam>) -> HashSet<Coord> {
    let mut already_energized = HashSet::new();
    for beam in already_seen {
        already_energized.insert(beam.origin);
    }
    already_energized
}

fn solve(input: &str) -> usize {
    let grid = build_grid(input);
    let start = Beam {
        origin: Coord { x: 0, y: 0 },
        dir: EAST,
    };

    let already_seen = calculate_propagation(&grid, start);
    let already_energized = calculate_energization(&already_seen);

    already_energized.len()
}

fn read_file(filename: &str) -> String {
    fs::read_to_string(filename).expect("Unable to read file")
}


fn main() {
    let input = read_file("input.txt");
    println!("{}", solve(&input));
}
