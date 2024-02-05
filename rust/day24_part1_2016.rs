
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

fn main() -> io::Result<()> {
    let input = read_file("./input.txt")?;
    let n = cleaning_robot(&input);
    println!("{}", n);
    Ok(())
}

fn cleaning_robot(input: &str) -> i32 {
    let grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    let mut graph: Vec<Vec<i32>> = vec![];
    for (r, row) in grid.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            if cell.is_digit(10) {
                let distances_from_poi = bfs_get_edge_weights(&grid, (r, c));
                if graph.is_empty() {
                    graph.resize(distances_from_poi.len(), vec![0; distances_from_poi.len()]);
                }
                let index = cell.to_digit(10).unwrap() as usize;
                graph[index] = distances_from_poi;
            }
        }
    }
    dfs(&graph, 0, &mut HashSet::new(), false)
}

fn bfs_get_edge_weights(grid: &[Vec<char>], start: (usize, usize)) -> Vec<i32> {
    let mut poi_to_distance: HashMap<char, i32> = HashMap::new();
    let mut queue: VecDeque<(usize, usize, i32)> = VecDeque::new();
    let mut visited: HashSet<(usize, usize)> = HashSet::new();
    let dirs = [(0, -1), (0, 1), (1, 0), (-1, 0)];

    queue.push_back((start.0, start.1, 0));
    while let Some((row, col, distance)) = queue.pop_front() {
        if !visited.insert((row, col)) {
            continue;
        }

        if grid[row][col].is_digit(10) {
            poi_to_distance.insert(grid[row][col], distance);
        }

        for &(dr, dc) in &dirs {
            let (next_row, next_col) = (row as isize + dr, col as isize + dc);
            if next_row >= 0
                && next_row < grid.len() as isize
                && next_col >= 0
                && next_col < grid[0].len() as isize
                && grid[next_row as usize][next_col as usize] != '#'
            {
                queue.push_back((next_row as usize, next_col as usize, distance + 1));
            }
        }
    }

    let mut distances = vec![0; poi_to_distance.len()];
    for (num, dist) in poi_to_distance {
        let n = num.to_digit(10).unwrap() as usize;
        distances[n] = dist;
    }
    distances
}

fn dfs(graph: &[Vec<i32>], entry_index: usize, visited: &mut HashSet<usize>, return_to_zero: bool) -> i32 {
    if graph.len() == visited.len() {
        return if return_to_zero { graph[entry_index][0] } else { 0 };
    }

    let mut min_distance = i32::MAX;
    for (i, &val) in graph[entry_index].iter().enumerate() {
        if !visited.contains(&i) {
            visited.insert(i);
            let dist = val + dfs(graph, i, visited, return_to_zero);
            min_distance = min_distance.min(dist);
            visited.remove(&i);
        }
    }

    min_distance
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents.trim_end().to_string())
}
