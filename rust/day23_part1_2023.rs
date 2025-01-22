
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
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
}

#[derive(Debug)]
struct Grid {
    width: i32,
    height: i32,
    data: HashMap<Coord, u8>,
}

const NORTH: Coord = Coord { x: 0, y: -1 };
const SOUTH: Coord = Coord { x: 0, y: 1 };
const WEST: Coord = Coord { x: -1, y: 0 };
const EAST: Coord = Coord { x: 1, y: 0 };

const EMPTY: u8 = b'.';
const WALL: u8 = b'#';
const NORTH_SLOPE: u8 = b'^';
const SOUTH_SLOPE: u8 = b'v';
const WEST_SLOPE: u8 = b'<';
const EAST_SLOPE: u8 = b'>';

fn is_in_bounds(grid: &Grid, coord: &Coord) -> bool {
    0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height
}

fn parse_input(input: &str) -> Grid {
    let lines: Vec<&str> = input.lines().collect();
    let height = lines.len() as i32;
    let width = lines[0].len() as i32;
    let mut data = HashMap::new();

    for (y, line) in lines.iter().enumerate() {
        for (x, char) in line.chars().enumerate() {
            let byte = char as u8;
            if byte != EMPTY {
                data.insert(Coord { x: x as i32, y: y as i32 }, byte);
            }
        }
    }

    Grid {
        width,
        height,
        data,
    }
}

fn is_valid_neighbor(grid: &Grid, coord: &Coord, dir: &Coord) -> bool {
    if !is_in_bounds(grid, coord) {
        return false;
    }
    if let Some(&value) = grid.data.get(coord) {
      if value == WALL {
        return false;
      }
    }

    true
}

fn is_valid_neighbor_with_slopes(grid: &Grid, coord: &Coord, dir: &Coord) -> bool {
    if !is_in_bounds(grid, coord) {
        return false;
    }
    let value = grid.data.get(coord);
     match value {
        Some(&WALL) => return false,
        Some(&NORTH_SLOPE) => return *dir == NORTH,
        Some(&SOUTH_SLOPE) => return *dir == SOUTH,
        Some(&WEST_SLOPE) => return *dir == WEST,
        Some(&EAST_SLOPE) => return *dir == EAST,
        _ => return true,
     }
}

fn neighbors4(
    grid: &Grid,
    coord: &Coord,
    is_valid_neighbor_func: fn(&Grid, &Coord, &Coord) -> bool,
) -> Vec<Coord> {
    let directions = [NORTH, SOUTH, WEST, EAST];
    let mut valid_neighbors = Vec::new();
    for dir in directions {
        let neighbor = coord.add(&dir);
        if is_valid_neighbor_func(grid, &neighbor, &dir) {
            valid_neighbors.push(neighbor);
        }
    }

    valid_neighbors
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
struct Edge {
    start: Coord,
    end: Coord,
    weight: i32,
}

#[derive(Debug)]
struct Graph {
    vertices: HashSet<Coord>,
    edges: HashMap<Coord, HashSet<Edge>>,
}

fn get_graph(
    grid: &Grid,
    start: &Coord,
    end: &Coord,
    is_valid_neighbor_func: fn(&Grid, &Coord, &Coord) -> bool,
) -> Graph {
    let mut vertices = HashSet::from([*start, *end]);

    for y in 0..grid.height {
        for x in 0..grid.width {
            let coord = Coord { x, y };
            if !grid.data.contains_key(&coord) {
                 if neighbors4(grid, &coord, is_valid_neighbor).len() > 2 {
                    vertices.insert(coord);
                }
            }
        }
    }

    let mut edges = HashMap::new();
    for start_node in &vertices {
        let node_edges = get_edges_bfs(grid, start_node, &vertices, is_valid_neighbor_func);
        edges.insert(*start_node, node_edges);
    }
    Graph { vertices, edges }
}


fn get_edges_bfs(
    grid: &Grid,
    start: &Coord,
    vertices: &HashSet<Coord>,
    is_valid_neighbor_func: fn(&Grid, &Coord, &Coord) -> bool,
) -> HashSet<Edge> {
    let mut frontier = VecDeque::from([*start]);
    let mut reached = HashSet::from([*start]);
    let mut distances = HashMap::from([(*start, 0)]);
    let mut edges = HashSet::new();

    while let Some(current) = frontier.pop_front() {
        if vertices.contains(&current) && current != *start {
            let edge = Edge {
                start: *start,
                end: current,
                weight: distances[&current],
            };
            edges.insert(edge);
            continue;
        }

        for next in neighbors4(grid, &current, is_valid_neighbor_func) {
            if !reached.contains(&next) {
                frontier.push_back(next);
                reached.insert(next);
                distances.insert(next, distances[&current] + 1);
            }
        }
    }

    edges
}


fn get_max_distance_dfs(
    grid: &Grid,
    graph: &Graph,
    current: &Coord,
    end: &Coord,
    seen: &mut HashSet<Coord>,
) -> Option<i32> {
    if current == end {
        return Some(0);
    }

    seen.insert(*current);
    let mut max_dist = 0;
    let mut found = false;

    if let Some(edges) = graph.edges.get(current) {
    for edge in edges {
        if !seen.contains(&edge.end) {
            if let Some(dist) = get_max_distance_dfs(grid, graph, &edge.end, end, seen){
                max_dist = max_dist.max(dist + edge.weight);
                found = true;
            }
        }
    }
    }
    seen.remove(current);

    if found {
      return Some(max_dist);
    } else {
        return None;
    }
}

fn solve(input: &str) -> i32 {
    let grid = parse_input(input);
    let start = Coord { x: 1, y: 0 };
    let end = Coord {
        x: grid.width - 2,
        y: grid.height - 1,
    };

    let graph = get_graph(&grid, &start, &end, is_valid_neighbor_with_slopes);
    get_max_distance_dfs(&grid, &graph, &start, &end, &mut HashSet::new()).unwrap()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    println!("{}", solve(&input));
}
