
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines();

    let patterns_line = lines.next().unwrap()?;
    let patterns: Vec<&str> = patterns_line.split(", ").collect();

    // Skip the blank line
    lines.next();

    let designs: Vec<String> = lines.map(|line| line.unwrap()).collect();

    let mut total_combinations = 0;
    for design in designs {
        let combinations = count_combinations(&design, &patterns, &mut HashMap::new());
        total_combinations += combinations;
    }

    println!("{}", total_combinations);

    Ok(())
}

fn count_combinations(design: &str, patterns: &[&str], memo: &mut HashMap<String, usize>) -> usize {
    if design.is_empty() {
        return 1;
    }

    if let Some(&count) = memo.get(design) {
        return count;
    }

    let mut total_count = 0;
    for pattern in patterns {
        if design.starts_with(pattern) {
            let remaining = &design[pattern.len()..];
            total_count += count_combinations(remaining, patterns, memo);
        }
    }

    memo.insert(design.to_string(), total_count);
    total_count
}
