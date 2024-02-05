
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut polymer = String::new();
    let mut rules = std::collections::HashMap::new();

    for (index, line) in reader.lines().enumerate() {
        let line = line.expect("Failed to read line");
        if index == 0 {
            polymer = line;
        } else {
            if line.is_empty() {
                continue;
            }
            let parts: Vec<&str> = line.split(" -> ").collect();
            rules.insert(parts[0].to_string(), parts[1].to_string());
        }
    }

    for _ in 0..10 {
        polymer = apply_insertion(&polymer, &rules);
    }

    let counts = count_elements(&polymer);
    let (min, max) = min_max(&counts);

    println!("{}", max - min);
}

fn apply_insertion(polymer: &str, rules: &std::collections::HashMap<String, String>) -> String {
    let mut new_polymer = String::new();
    for i in 0..polymer.len() - 1 {
        new_polymer.push_str(&polymer[i..=i]);
        if let Some(insert) = rules.get(&polymer[i..=i+1]) {
            new_polymer.push_str(insert);
        }
    }
    new_polymer.push_str(&polymer[polymer.len() - 1..]);
    new_polymer
}

fn count_elements(polymer: &str) -> std::collections::HashMap<char, i32> {
    let mut counts = std::collections::HashMap::new();
    for c in polymer.chars() {
        *counts.entry(c).or_insert(0) += 1;
    }
    counts
}

fn min_max(counts: &std::collections::HashMap<char, i32>) -> (i32, i32) {
    let mut min = i32::MAX;
    let mut max = i32::MIN;
    for &count in counts.values() {
        if count < min {
            min = count;
        }
        if count > max {
            max = count;
        }
    }
    (min, max)
}
