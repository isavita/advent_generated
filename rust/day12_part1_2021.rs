
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead};

fn is_small_cave(cave: &str) -> bool {
    cave.chars().all(|c| c.is_ascii_lowercase())
}

fn find_paths(
    graph: &HashMap<String, Vec<String>>,
    current: String,
    path: &mut Vec<String>,
    paths: &mut Vec<Vec<String>>,
    visited_small_caves: &mut HashSet<String>,
) {
    path.push(current.clone());

    if current == "end" {
        paths.push(path.clone());
        path.pop();
        return;
    }

    if is_small_cave(&current) {
        visited_small_caves.insert(current.clone());
    }


    if let Some(neighbors) = graph.get(&current) {
        for neighbor in neighbors {
            if !is_small_cave(neighbor) || !visited_small_caves.contains(neighbor) {
               
                find_paths(graph, neighbor.clone(), path, paths, visited_small_caves);
            }
        }
    }

    if is_small_cave(&current) {
        visited_small_caves.remove(&current);
    }
    path.pop();
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut graph: HashMap<String, Vec<String>> = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split('-').collect();
        if parts.len() == 2 {
            let (from, to) = (parts[0].to_string(), parts[1].to_string());
            graph.entry(from.clone()).or_default().push(to.clone());
            graph.entry(to).or_default().push(from);
        }
    }

    let mut paths: Vec<Vec<String>> = Vec::new();
    let mut path: Vec<String> = Vec::new();
    let mut visited_small_caves: HashSet<String> = HashSet::new();
    find_paths(&graph, "start".to_string(), &mut path, &mut paths, &mut visited_small_caves);

    println!("{}", paths.len());

    Ok(())
}
