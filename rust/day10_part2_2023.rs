
use std::{
    collections::{HashMap, HashSet},
    fs,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

    fn opposite(&self) -> Coord {
        Coord {
            x: -self.x,
            y: -self.y,
        }
    }
}

type Tile = char;
type Pipe = HashSet<Coord>;

struct Grid {
    width: usize,
    height: usize,
    data: HashMap<Coord, Tile>,
}

const UNDEFINED: Coord = Coord { x: 0, y: 0 };
const TOP: Coord = Coord { x: 0, y: -1 };
const RIGHT: Coord = Coord { x: 1, y: 0 };
const BOTTOM: Coord = Coord { x: 0, y: 1 };
const LEFT: Coord = Coord { x: -1, y: 0 };

const EMPTY: Tile = '.';
const START: Tile = 'S';
const VERTICAL: Tile = '|';
const HORIZONTAL: Tile = '-';
const TOP_LEFT_CORNER: Tile = 'J';
const TOP_RIGHT_CORNER: Tile = 'L';
const BOTTOM_LEFT_CORNER: Tile = '7';
const BOTTOM_RIGHT_CORNER: Tile = 'F';
const ENCLOSED: Tile = 'X';

fn get_pipe_from_tile(tile: Tile) -> Pipe {
    match tile {
        VERTICAL => HashSet::from([TOP, BOTTOM]),
        HORIZONTAL => HashSet::from([LEFT, RIGHT]),
        TOP_LEFT_CORNER => HashSet::from([TOP, LEFT]),
        TOP_RIGHT_CORNER => HashSet::from([TOP, RIGHT]),
        BOTTOM_LEFT_CORNER => HashSet::from([BOTTOM, LEFT]),
        BOTTOM_RIGHT_CORNER => HashSet::from([BOTTOM, RIGHT]),
        _ => HashSet::new(),
    }
}

fn get_tile_from_pipe(pipe: &Pipe) -> Tile {
    if pipe == &HashSet::from([TOP, BOTTOM]) {
        VERTICAL
    } else if pipe == &HashSet::from([LEFT, RIGHT]) {
        HORIZONTAL
    } else if pipe == &HashSet::from([TOP, LEFT]) {
        TOP_LEFT_CORNER
    } else if pipe == &HashSet::from([TOP, RIGHT]) {
        TOP_RIGHT_CORNER
    } else if pipe == &HashSet::from([BOTTOM, LEFT]) {
        BOTTOM_LEFT_CORNER
    } else if pipe == &HashSet::from([BOTTOM, RIGHT]) {
        BOTTOM_RIGHT_CORNER
    } else {
        EMPTY
    }
}

fn build_grid(input: &str) -> Grid {
    let lines: Vec<&str> = input.lines().collect();
    let width = lines[0].len();
    let height = lines.len();
    let mut data = HashMap::new();

    for (y, line) in lines.iter().enumerate() {
        for (x, char) in line.chars().enumerate() {
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

fn find_start(grid: &Grid) -> Coord {
    *grid.data.iter().find(|(_, &value)| value == START).unwrap().0
}

fn get_pipe_from_neighbors(c: &Coord, grid: &Grid) -> Pipe {
    let possible_neighbors = [TOP, RIGHT, BOTTOM, LEFT];
    let mut pipe = HashSet::new();

    for dir in possible_neighbors {
        let neighbor_coord = c.add(&dir);
         if let Some(&neighbor_tile) = grid.data.get(&neighbor_coord) {
            let neighbor_pipe = get_pipe_from_tile(neighbor_tile);
            if neighbor_pipe.contains(&dir.opposite()) {
                pipe.insert(dir);
            }
        }
    }
    pipe
}

fn path_finding(start: &Coord, grid: &Grid) -> Vec<Coord> {
    let mut path = vec![*start];
    let start_pipe = get_pipe_from_neighbors(start, grid);
    let mut previous_dir = *start_pipe.iter().next().unwrap();
    let mut current = start.add(&previous_dir);

    while current != *start {
        path.push(current);
        let current_pipe = get_pipe_from_tile(*grid.data.get(&current).unwrap());
        for dir in current_pipe {
            if dir != previous_dir.opposite() {
                previous_dir = dir;
                current = current.add(&dir);
                break;
            }
        }
    }

    path
}

fn get_path_grid(grid: &Grid, path: &[Coord], empty: Tile) -> Grid {
    let mut new_grid = Grid {
        width: grid.width,
        height: grid.height,
        data: HashMap::new(),
    };

    for coord in path {
        new_grid.data.insert(*coord, *grid.data.get(coord).unwrap());
    }

    let start = path[0];
    new_grid.data.insert(start, get_tile_from_pipe(&get_pipe_from_neighbors(&start, grid)));
    new_grid
}

fn is_inside(c: &Coord, grid: &Grid, empty: Tile) -> bool {
     if grid.data.contains_key(c) {
        return false;
    }

    let mut start_pipe = empty;
    let mut num_pipe_on_left = 0;

    for x in 0..c.x {
        let coord = Coord { x, y: c.y };
         if let Some(&v) = grid.data.get(&coord) {
            match v {
                VERTICAL => num_pipe_on_left += 1,
                TOP_RIGHT_CORNER => start_pipe = TOP_RIGHT_CORNER,
                BOTTOM_RIGHT_CORNER => start_pipe = BOTTOM_RIGHT_CORNER,
                TOP_LEFT_CORNER => {
                    if start_pipe == BOTTOM_RIGHT_CORNER {
                        start_pipe = empty;
                        num_pipe_on_left += 1;
                    } else if v == TOP_RIGHT_CORNER {
                       start_pipe = empty;
                    }
                }
                BOTTOM_LEFT_CORNER => {
                    if start_pipe == TOP_RIGHT_CORNER {
                       start_pipe = empty;
                        num_pipe_on_left += 1;
                    } else if start_pipe == BOTTOM_RIGHT_CORNER {
                        start_pipe = empty;
                    }
                }
                _ => (),
            }
        }
    }
    num_pipe_on_left % 2 == 1
}


fn solve(input: &str) -> usize {
    let grid = build_grid(input);
    let start = find_start(&grid);
    let path = path_finding(&start, &grid);
    let path_grid = get_path_grid(&grid, &path, EMPTY);

    let mut cnt = 0;
    for y in 0..grid.height as i32 {
        for x in 0..grid.width as i32 {
            let c = Coord { x, y };
            if is_inside(&c, &path_grid, EMPTY) {
                cnt += 1;
            }
        }
    }
    cnt
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    println!("{}", solve(&input));
}
