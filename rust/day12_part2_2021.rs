use std::fs::File;
use std::io::{self, BufRead};
use std::collections::{HashMap, HashSet};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines: Vec<String> = io::BufReader::new(file).lines()
        .map(|line| line.expect("Could not parse line"))
        .collect();

    let parsed = parse_input(lines);
    let mut graph: HashMap<String, HashMap<String, bool>> = HashMap::new();
    for pair in parsed {
        graph.entry(pair.0.clone()).or_insert(HashMap::new());
        graph.entry(pair.1.clone()).or_insert(HashMap::new());
        graph.get_mut(&pair.0).unwrap().insert(pair.1.clone(), true);
        graph.get_mut(&pair.1).unwrap().insert(pair.0.clone(), true);
    }

    let ans = walk(&graph, "start", HashMap::new(), vec!["start".to_string()], false);
    println!("{}", ans);

    Ok(())
}

fn walk(graph: &HashMap<String, HashMap<String, bool>>, current: &str, visited: HashMap<String, i32>, path: Vec<String>, double_used: bool) -> i32 {
    if current == "end" {
        return 1;
    }

    let mut visited = visited.clone();
    *visited.entry(current.to_string()).or_insert(0) += 1;

    let mut paths_to_end = 0;

    for (visitable, _) in graph.get(current).unwrap() {
        if visitable == "start" {
            continue;
        }

        if visitable.chars().all(|c| c.is_uppercase()) || *visited.get(visitable).unwrap_or(&0) == 0 {
            let mut path = path.clone();
            path.push(visitable.clone());
            paths_to_end += walk(graph, visitable, visited.clone(), path, double_used);
        } else if !double_used {
            let mut path = path.clone();
            path.push(visitable.clone());
            paths_to_end += walk(graph, visitable, visited.clone(), path, true);
        }
    }

    *visited.get_mut(current).unwrap() -= 1;

    paths_to_end
}

fn parse_input(input: Vec<String>) -> Vec<(String, String)> {
    input.into_iter().filter_map(|line| {
        let mut parts = line.split("-");
        let a = parts.next()?;
        let b = parts.next()?;
        Some((a.to_string(), b.to_string()))
    }).collect()
}