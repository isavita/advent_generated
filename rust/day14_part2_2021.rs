use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let (template, rules) = read_input("input.txt");
    let mut pair_counts: HashMap<String, i64> = HashMap::new();

    for i in 0..template.len() - 1 {
        let pair = &template[i..i + 2];
        *pair_counts.entry(pair.to_string()).or_insert(0) += 1;
    }

    for _ in 0..40 {
        let mut new_pair_counts: HashMap<String, i64> = HashMap::new();
        for (pair, count) in &pair_counts {
            if let Some(insert) = rules.get(pair) {
                *new_pair_counts.entry(pair.chars().take(1).chain(insert.chars()).collect()).or_insert(0) += count;
                *new_pair_counts.entry(insert.chars().chain(pair.chars().skip(1)).collect()).or_insert(0) += count;
            } else {
                *new_pair_counts.entry(pair.to_string()).or_insert(0) += count;
            }
        }
        pair_counts = new_pair_counts;
    }

    let mut element_counts: HashMap<char, i64> = HashMap::new();
    for (pair, count) in &pair_counts {
        *element_counts.entry(pair.chars().next().unwrap()).or_insert(0) += count;
    }
    *element_counts.entry(template.chars().last().unwrap()).or_insert(0) += 1;

    let mut max_count = 0;
    let mut min_count = i64::MAX;
    for &count in element_counts.values() {
        if count > max_count {
            max_count = count;
        }
        if count < min_count {
            min_count = count;
        }
    }

    println!("{}", max_count - min_count);
}

fn read_input(filename: &str) -> (String, HashMap<String, String>) {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let template = lines.next().unwrap().unwrap();

    let mut rules: HashMap<String, String> = HashMap::new();
    for line in lines.filter_map(Result::ok) {
        if line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split(" -> ").collect();
        rules.insert(parts[0].to_string(), parts[1].to_string());
    }

    (template, rules)
}