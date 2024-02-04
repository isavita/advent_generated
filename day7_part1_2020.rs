
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let rules: Vec<&str> = input.trim().split('\n').collect();

    let mut bag_map = std::collections::HashMap::new();
    for rule in rules {
        let parts: Vec<&str> = rule.split(" bags contain ").collect();
        let outer_bag = parts[0];
        let inner_bags: Vec<&str> = parts[1].split(", ").collect();
        for bag in inner_bags {
            if bag != "no other bags." {
                let bag_parts: Vec<&str> = bag.splitn(2, ' ').collect();
                let count: usize = bag_parts[0].parse().unwrap();
                let inner_bag = bag_parts[1].rsplitn(2, ' ').nth(1).unwrap();
                bag_map.entry(inner_bag).or_insert(vec![]).push(outer_bag);
            }
        }
    }

    let mut count = 0;
    let mut stack = vec!["shiny gold"];
    let mut visited = std::collections::HashSet::new();

    while let Some(bag) = stack.pop() {
        if let Some(outer_bags) = bag_map.get(bag) {
            for outer_bag in outer_bags {
                if !visited.contains(outer_bag) {
                    visited.insert(outer_bag);
                    stack.push(outer_bag);
                    count += 1;
                }
            }
        }
    }

    println!("{}", count);
}
