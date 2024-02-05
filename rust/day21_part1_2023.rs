
use std::fs;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
}

struct Grid {
    width: i32,
    height: i32,
    data: std::collections::HashMap<Coord, char>,
}

const NORTH: Coord = Coord { x: 0, y: -1 };
const WEST: Coord = Coord { x: -1, y: 0 };
const SOUTH: Coord = Coord { x: 0, y: 1 };
const EAST: Coord = Coord { x: 1, y: 0 };

const EMPTY: char = '.';
const ROCK: char = '#';
const START: char = 'S';

fn is_in_bounds(grid: &Grid, coord: Coord) -> bool {
    0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height
}

fn parse_input(input: Vec<String>) -> Grid {
    let mut data = std::collections::HashMap::new();

    for (y, line) in input.iter().enumerate() {
        for (x, char) in line.chars().enumerate() {
            if char != EMPTY {
                data.insert(Coord { x: x as i32, y: y as i32 }, char);
            }
        }
    }

    Grid {
        width: input[0].len() as i32,
        height: input.len() as i32,
        data,
    }
}

fn find_start(grid: &Grid) -> Coord {
    for (coord, &char) in &grid.data {
        if char == START {
            return *coord;
        }
    }
    panic!("No start found.");
}

fn neighbors4(grid: &Grid, coord: Coord) -> Vec<Coord> {
    let neighbors = vec![
        coord.add(NORTH),
        coord.add(SOUTH),
        coord.add(EAST),
        coord.add(WEST),
    ];

    let mut valid_neighbors = vec![];

    for neighbor in neighbors {
        if is_in_bounds(grid, neighbor) && grid.data.get(&neighbor) != Some(&ROCK) {
            valid_neighbors.push(neighbor);
        }
    }

    valid_neighbors
}

fn breadth_first_search(grid: &Grid, start: Coord, neighbor_func: fn(&Grid, Coord) -> Vec<Coord>) -> std::collections::HashMap<Coord, i32> {
    let mut frontier = vec![start];
    let mut reached = std::collections::HashMap::new();
    reached.insert(start, ());
    let mut came_from = std::collections::HashMap::new();
    came_from.insert(start, start);
    let mut distances = std::collections::HashMap::new();
    distances.insert(start, 0);

    while !frontier.is_empty() {
        let current = frontier.remove(0);

        for next in neighbor_func(grid, current) {
            if !reached.contains_key(&next) {
                frontier.push(next);
                reached.insert(next, ());
                came_from.insert(next, current);
                distances.insert(next, distances[&current] + 1);
            }
        }
    }

    distances
}

fn distances_from_extremities(grid: &Grid) -> std::collections::HashMap<Coord, std::collections::HashMap<Coord, i32>> {
    let mut distances = std::collections::HashMap::new();

    let extremities = vec![
        Coord { x: 0, y: 0 },
        Coord { x: grid.width / 2, y: 0 },
        Coord { x: grid.width, y: 0 },
        Coord { x: grid.width, y: grid.height / 2 },
        Coord { x: grid.width, y: grid.height },
        Coord { x: grid.width / 2, y: grid.height },
        Coord { x: 0, y: grid.height },
        Coord { x: 0, y: grid.height / 2 },
    ];

    for &start in &extremities {
        distances.insert(start, breadth_first_search(grid, start, neighbors4));
    }

    distances
}

fn neighbors8(grid: &Grid, coord: Coord) -> Vec<Coord> {
    let neighbors = vec![
        coord.add(NORTH),
        coord.add(SOUTH),
        coord.add(EAST),
        coord.add(WEST),
        coord.add(NORTH).add(EAST),
        coord.add(NORTH).add(WEST),
        coord.add(SOUTH).add(EAST),
        coord.add(SOUTH).add(WEST),
    ];

    neighbors
}

fn solve(input: Vec<String>, num_steps: i32) -> i32 {
    let grid = parse_input(input);

    let start = find_start(&grid);
    let distances = breadth_first_search(&grid, start, neighbors4);

    let mut cnt = 0;
    for &dist in distances.values() {
        if dist <= num_steps && dist % 2 == 0 {
            cnt += 1;
        }
    }
    cnt
}

fn read_file(file_name: &str) -> Vec<String> {
    let content = fs::read_to_string(file_name).expect("Error reading the file");
    content.trim().split('\n').map(|s| s.to_string()).collect()
}

fn main() {
    let input = read_file("input.txt");
    println!("{}", solve(input, 64));
}
