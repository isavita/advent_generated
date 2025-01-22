
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::i32;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let n = cleaning_robot(&input);
    println!("{}", n);
}

fn cleaning_robot(input: &str) -> i32 {
    let grid: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    let mut graph: Vec<Vec<i32>> = Vec::new();
    for (r, row) in grid.iter().enumerate() {
        for (c, &cell) in row.iter().enumerate() {
            if cell.is_ascii_digit() {
                let distances_from_poi = bfs_get_edge_weights(&grid, (r, c));

                if graph.is_empty() {
                    graph.resize(distances_from_poi.len(), vec![0; distances_from_poi.len()]);
                }
                let index = cell.to_digit(10).unwrap() as usize;
                graph[index] = distances_from_poi;
            }
        }
    }

    dfs(&graph, 0, &mut vec![false; graph.len()], true)
}

fn bfs_get_edge_weights(grid: &Vec<Vec<char>>, start: (usize, usize)) -> Vec<i32> {
    let mut poi_to_distance: HashMap<char, i32> = HashMap::new();
    poi_to_distance.insert(grid[start.0][start.1], 0);

    let mut queue: VecDeque<(usize, usize, i32)> = VecDeque::new();
    queue.push_back((start.0, start.1, 0));

    let mut visited: HashMap<(usize, usize), bool> = HashMap::new();
    while let Some((row, col, distance)) = queue.pop_front() {
        if visited.contains_key(&(row, col)) {
            continue;
        }
        visited.insert((row, col), true);

        if grid[row][col].is_ascii_digit() {
            poi_to_distance.insert(grid[row][col], distance);
        }
        let dirs: [(i32, i32); 4] = [(0, -1), (0, 1), (1, 0), (-1, 0)];
        for (dr, dc) in dirs.iter() {
            let next_row = (row as i32 + dr) as usize;
            let next_col = (col as i32 + dc) as usize;
            if next_row < grid.len() && next_col < grid[0].len() && grid[next_row][next_col] != '#' {
                queue.push_back((next_row, next_col, distance + 1));
            }
        }
    }

    let mut distances: Vec<i32> = vec![0; poi_to_distance.len()];
    for (num_char, dist) in poi_to_distance {
        let n = num_char.to_digit(10).unwrap() as usize;
        distances[n] = dist;
    }
    distances
}

fn dfs(graph: &Vec<Vec<i32>>, entry_index: usize, visited: &mut Vec<bool>, return_to_zero: bool) -> i32 {
    if visited.iter().all(|&v| v) {
        if return_to_zero {
            return graph[entry_index][0];
        }
        return 0;
    }

    let mut min_distance = i32::MAX;
    for (i, &val) in graph[entry_index].iter().enumerate() {
        if !visited[i] {
            visited[i] = true;
            let dist = val + dfs(graph, i, visited, return_to_zero);
            min_distance = min_distance.min(dist);
            visited[i] = false;
        }
    }
    min_distance
}
