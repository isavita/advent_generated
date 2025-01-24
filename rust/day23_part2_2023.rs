
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
}

fn solve(filename: &str, ignore_slopes: bool) -> usize {
    let mut grid: Vec<Vec<char>> = Vec::new();
    if let Ok(lines) = read_lines(filename) {
        for line in lines {
            if let Ok(row) = line {
                grid.push(row.chars().collect());
            }
        }
    }

    let start = Point::new(1, 0);
    let end = Point::new(grid[0].len() as i32 - 2, grid.len() as i32 - 1);

    let mut graph = build_graph(&grid, ignore_slopes);

    longest_path(&graph, start, end)
}

fn build_graph(grid: &Vec<Vec<char>>, ignore_slopes: bool) -> HashMap<Point, Vec<(Point, usize)>> {
    let mut graph: HashMap<Point, Vec<(Point, usize)>> = HashMap::new();
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;

    let mut nodes = Vec::new();
    nodes.push(Point::new(1, 0));
    nodes.push(Point::new(cols - 2, rows - 1));
    for r in 0..rows {
        for c in 0..cols {
            if grid[r as usize][c as usize] != '#' {
                let mut neighbors = 0;
                for &(dr, dc) in &[(-1, 0), (1, 0), (0, -1), (0, 1)] {
                    let nr = r + dr;
                    let nc = c + dc;
                    if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr as usize][nc as usize] != '#' {
                        neighbors += 1;
                    }
                }
                if neighbors > 2 {
                    nodes.push(Point::new(c, r));
                }
            }
        }
    }

    for &node in &nodes {
        let mut stack = VecDeque::new();
        stack.push_back((node, 0));
        let mut visited = HashSet::new();
        visited.insert(node);
        
        while let Some((curr, dist)) = stack.pop_front() {
            if dist != 0 && nodes.contains(&curr) {
                graph.entry(node).or_default().push((curr, dist));
                continue;
            }

            let directions: &[(i32, i32)] = if ignore_slopes {
                &[(-1, 0), (1, 0), (0, -1), (0, 1)]
            } else {
                match grid[curr.y as usize][curr.x as usize] {
                    '^' => &[(-1, 0)],
                    'v' => &[(1, 0)],
                    '<' => &[(0, -1)],
                    '>' => &[(0, 1)],
                    _ => &[(-1, 0), (1, 0), (0, -1), (0, 1)],
                }
            };
            
            for &(dr, dc) in directions {
                let nr = curr.y + dr;
                let nc = curr.x + dc;
                
                if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr as usize][nc as usize] != '#' {
                    let next = Point::new(nc, nr);
                    if !visited.contains(&next) {
                        visited.insert(next);
                        stack.push_back((next, dist + 1));
                    }
                }
            }
        }
    }

    graph
}

fn longest_path(graph: &HashMap<Point, Vec<(Point, usize)>>, start: Point, end: Point) -> usize {
    let mut max_dist = 0;
    let mut stack = VecDeque::new();
    stack.push_back((start, HashSet::from([start]), 0));

    while let Some((curr, visited, dist)) = stack.pop_back() {
        if curr == end {
            max_dist = max_dist.max(dist);
            continue;
        }
        if let Some(neighbors) = graph.get(&curr) {
            for &(next, weight) in neighbors {
                if !visited.contains(&next) {
                    let mut new_visited = visited.clone();
                    new_visited.insert(next);
                    stack.push_back((next, new_visited, dist + weight));
                }
            }
        }
    }

    max_dist
}

fn main() {
    let filename = "input.txt";
    let part1_solution = solve(filename, false);
    println!("Part 1: {}", part1_solution);

    let part2_solution = solve(filename, true);
    println!("Part 2: {}", part2_solution);
}
