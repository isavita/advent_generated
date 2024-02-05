
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    fn add(&self, other: Coord) -> Coord {
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
        (0..grid.width).contains(&self.x) && (0..grid.height).contains(&self.y)
    }
}

#[derive(Debug)]
struct Grid {
    width: i32,
    height: i32,
    data: std::collections::HashMap<Coord, char>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Beam {
    origin: Coord,
    dir: Coord,
}

const EMPTY: char = '.';
const ASCENDING_MIRROR: char = '/';
const DESCENDING_MIRROR: char = '\\';
const VERTICAL_SPLITTER: char = '|';
const HORIZONTAL_SPLITTER: char = '-';

const NORTH: Coord = Coord { x: 0, y: -1 };
const WEST: Coord = Coord { x: -1, y: 0 };
const SOUTH: Coord = Coord { x: 0, y: 1 };
const EAST: Coord = Coord { x: 1, y: 0 };

fn build_grid(input: Vec<String>) -> Grid {
    let mut data = std::collections::HashMap::new();
    let height = input.len() as i32;
    let width = input[0].len() as i32;

    for (y, line) in input.iter().enumerate() {
        for (x, &char) in line.as_bytes().iter().enumerate() {
            if char != EMPTY as u8 {
                data.insert(Coord { x: x as i32, y: y as i32 }, char as char);
            }
        }
    }

    Grid { width, height, data }
}

fn next_beam(grid: &Grid, beam: Beam) -> Vec<Beam> {
    let mut beams = Vec::new();
    if let Some(&char) = grid.data.get(&beam.origin) {
        match char {
            ASCENDING_MIRROR => {
                let new_dir = if beam.dir == NORTH || beam.dir == SOUTH {
                    beam.dir.rotate_neg90()
                } else {
                    beam.dir.rotate90()
                };
                let new_beam = Beam {
                    origin: beam.origin.add(new_dir),
                    dir: new_dir,
                };
                beams.push(new_beam);
            }
            DESCENDING_MIRROR => {
                let new_dir = if beam.dir == NORTH || beam.dir == SOUTH {
                    beam.dir.rotate90()
                } else {
                    beam.dir.rotate_neg90()
                };
                let new_beam = Beam {
                    origin: beam.origin.add(new_dir),
                    dir: new_dir,
                };
                beams.push(new_beam);
            }
            VERTICAL_SPLITTER if beam.dir == EAST || beam.dir == WEST => {
                let new_dir1 = beam.dir.rotate90();
                let new_beam1 = Beam {
                    origin: beam.origin.add(new_dir1),
                    dir: new_dir1,
                };
                let new_dir2 = beam.dir.rotate_neg90();
                let new_beam2 = Beam {
                    origin: beam.origin.add(new_dir2),
                    dir: new_dir2,
                };
                beams.push(new_beam1);
                beams.push(new_beam2);
            }
            HORIZONTAL_SPLITTER if beam.dir == NORTH || beam.dir == SOUTH => {
                let new_dir1 = beam.dir.rotate90();
                let new_beam1 = Beam {
                    origin: beam.origin.add(new_dir1),
                    dir: new_dir1,
                };
                let new_dir2 = beam.dir.rotate_neg90();
                let new_beam2 = Beam {
                    origin: beam.origin.add(new_dir2),
                    dir: new_dir2,
                };
                beams.push(new_beam1);
                beams.push(new_beam2);
            }
            _ => {
                let new_beam = Beam {
                    origin: beam.origin.add(beam.dir),
                    dir: beam.dir,
                };
                beams.push(new_beam);
            }
        }
    } else {
        let new_beam = Beam {
            origin: beam.origin.add(beam.dir),
            dir: beam.dir,
        };
        beams.push(new_beam);
    }

    beams
}

fn calculate_propagation(grid: &Grid, start: Beam) -> std::collections::HashSet<Beam> {
    let mut already_seen = std::collections::HashSet::new();
    let mut to_explore = vec![start];

    while let Some(beam) = to_explore.pop() {
        if beam.origin.is_in_bounds(grid) && !already_seen.contains(&beam) {
            already_seen.insert(beam);
            to_explore.extend(next_beam(grid, beam));
        }
    }

    already_seen
}

fn build_beam_grid(grid: &Grid, already_seen: &std::collections::HashSet<Beam>) -> Grid {
    let mut beam_grid = Grid {
        width: grid.width,
        height: grid.height,
        data: std::collections::HashMap::new(),
    };

    for (&coord, &char) in &grid.data {
        beam_grid.data.insert(coord, char);
    }

    for &beam in already_seen {
        if !grid.data.contains_key(&beam.origin) {
            if beam_grid.data.contains_key(&beam.origin) {
                beam_grid.data.insert(beam.origin, '2');
            } else {
                let beam_char = match beam.dir {
                    NORTH => '^',
                    EAST => '>',
                    SOUTH => 'v',
                    WEST => '<',
                    _ => unreachable!(),
                };
                beam_grid.data.insert(beam.origin, beam_char);
            }
        }
    }

    beam_grid
}

fn calculate_energization(already_seen: &std::collections::HashSet<Beam>) -> std::collections::HashSet<Coord> {
    already_seen.iter().map(|beam| beam.origin).collect()
}

fn get_border(grid: &Grid) -> Vec<Beam> {
    let mut border = Vec::new();

    for x in 0..grid.width {
        let coord = Coord { x, y: 0 };
        border.push(Beam { origin: coord, dir: SOUTH });

        let coord = Coord { x, y: grid.height - 1 };
        border.push(Beam { origin: coord, dir: NORTH });
    }

    for y in 0..grid.height {
        let coord = Coord { x: 0, y };
        border.push(Beam { origin: coord, dir: EAST });

        let coord = Coord { x: grid.width - 1, y };
        border.push(Beam { origin: coord, dir: WEST });
    }

    border
}

fn solve(input: Vec<String>) -> usize {
    let grid = build_grid(input);
    let starts = get_border(&grid);

    let mut res = 0;
    for &start in &starts {
        let already_seen = calculate_propagation(&grid, start);
        let already_energized = calculate_energization(&already_seen);

        let energy = already_energized.len();
        if energy > res {
            res = energy;
        }
    }

    res
}

fn read_file(file_name: &str) -> Vec<String> {
    fs::read_to_string(file_name)
        .expect("Error reading file")
        .lines()
        .map(|line| line.to_string())
        .collect()
}

fn main() {
    let input = read_file("input.txt");
    println!("{}", solve(input));
}
