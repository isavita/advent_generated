
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let rules = io::BufReader::new(file)
        .lines()
        .map(|line| line.unwrap())
        .collect::<Vec<String>>();

    let (contains_map, contained_by_map) = parse_rules(&rules);
    println!("Part 1: {}", part1(&contained_by_map));
    println!("Part 2: {}", part2(&contains_map));

    Ok(())
}

fn parse_rules(
    rules: &Vec<String>,
) -> (
    HashMap<String, Vec<(usize, String)>>,
    HashMap<String, Vec<String>>,
) {
    let mut contains_map: HashMap<String, Vec<(usize, String)>> = HashMap::new();
    let mut contained_by_map: HashMap<String, Vec<String>> = HashMap::new();
    for rule in rules {
        let parts: Vec<&str> = rule.split(" bags contain ").collect();
        let container = parts[0].to_string();
        let contents_str = parts[1];

        if contents_str == "no other bags." {
            continue;
        }

        for content_str in contents_str.split(", ") {
            let content_parts: Vec<&str> = content_str.split(" ").collect();
            let count: usize = content_parts[0].parse().unwrap();
            let content = format!("{} {} ", content_parts[1], content_parts[2]);
            let content = content.trim().to_string();


            contains_map.entry(container.clone()).or_default().push((count, content.clone()));
            contained_by_map.entry(content).or_default().push(container.clone());
        }
    }
    (contains_map, contained_by_map)
}

fn part1(contained_by_map: &HashMap<String, Vec<String>>) -> usize {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    queue.push_back("shiny gold".to_string());
    visited.insert("shiny gold".to_string());

    while let Some(bag) = queue.pop_front() {
        if let Some(containers) = contained_by_map.get(&bag) {
            for container in containers {
                if visited.insert(container.clone()) {
                    queue.push_back(container.clone());
                }
            }
        }
    }
    visited.len() - 1
}


fn part2(contains_map: &HashMap<String, Vec<(usize, String)>>) -> usize {
    let mut queue = VecDeque::new();
    queue.push_back(("shiny gold".to_string(), 1));
    let mut total_bags = 0;

    while let Some((bag, count)) = queue.pop_front() {
        if let Some(contents) = contains_map.get(&bag) {
            for (content_count, content_bag) in contents {
                total_bags += count * content_count;
                queue.push_back((content_bag.clone(), count * content_count));
            }
        }
    }
    total_bags
}
