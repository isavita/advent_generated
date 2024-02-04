
use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.trim().split('\n').collect();
    let mut graph: HashMap<i32, HashSet<i32>> = HashMap::new();

    for line in lines {
        let parts: Vec<&str> = line.split(" <-> ").collect();
        let node: i32 = parts[0].parse().unwrap();
        let neighbors: HashSet<i32> = parts[1]
            .split(", ")
            .map(|x| x.parse().unwrap())
            .collect();
        graph.insert(node, neighbors);
    }

    let mut visited: HashSet<i32> = HashSet::new();
    let mut groups = 0;

    for node in graph.keys() {
        if visited.contains(node) {
            continue;
        }

        groups += 1;
        let mut stack: Vec<i32> = vec![*node];

        while let Some(current) = stack.pop() {
            if visited.contains(&current) {
                continue;
            }

            visited.insert(current);
            if let Some(neighbors) = graph.get(&current) {
                for &neighbor in neighbors {
                    stack.push(neighbor);
                }
            }
        }
    }

    println!("{}", groups);
}
