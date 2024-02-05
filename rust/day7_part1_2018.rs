
use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let mut dependencies: HashMap<char, HashSet<char>> = HashMap::new();
    let mut steps: HashSet<char> = HashSet::new();

    for line in input.lines() {
        let parts: Vec<char> = line.chars().collect();
        let step = parts[36];
        let dependency = parts[5];

        dependencies.entry(step).or_insert(HashSet::new()).insert(dependency);
        dependencies.entry(dependency).or_insert(HashSet::new());
        steps.insert(step);
        steps.insert(dependency);
    }

    let mut order: Vec<char> = Vec::new();

    while !steps.is_empty() {
        let mut available: Vec<char> = steps.iter().filter(|&step| dependencies[step].is_empty()).cloned().collect();
        available.sort();
        let next_step = available[0];
        order.push(next_step);
        steps.remove(&next_step);
        for deps in dependencies.values_mut() {
            deps.remove(&next_step);
        }
    }

    println!("{}", order.iter().collect::<String>());
}
