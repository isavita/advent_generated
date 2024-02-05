
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut pipes: Vec<Vec<usize>> = vec![vec![]; 2000];

    for line in input.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let program_id: usize = parts[0].parse().unwrap();
        for other_id in parts[2..].iter() {
            let other_id: usize = other_id.trim_matches(|c| c == ',' || c == '\n').parse().unwrap();
            pipes[program_id].push(other_id);
            pipes[other_id].push(program_id);
        }
    }

    let mut visited: Vec<bool> = vec![false; 2000];
    let mut group_size = 0;

    fn dfs(pipes: &Vec<Vec<usize>>, visited: &mut Vec<bool>, node: usize) {
        visited[node] = true;
        for &neighbor in pipes[node].iter() {
            if !visited[neighbor] {
                dfs(pipes, visited, neighbor);
            }
        }
    }

    dfs(&pipes, &mut visited, 0);

    for v in visited {
        if v {
            group_size += 1;
        }
    }

    println!("{}", group_size);
}
