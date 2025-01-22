
use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead},
};

#[derive(Debug)]
struct Row {
    springs: String,
    group: Vec<usize>,
}

fn parse_input(input: &str) -> Vec<Row> {
    input
        .lines()
        .map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            let springs = parts[0].to_string();
            let group = parts[1]
                .split(',')
                .map(|s| s.parse().unwrap())
                .collect();
            Row { springs, group }
        })
        .collect()
}

fn count_arrangements_recursive(
    row: &Row,
    i_springs: usize,
    i_group: usize,
    i_contiguous_damaged: usize,
    cache: &mut HashMap<(usize, usize, usize), usize>,
) -> usize {
    if i_springs == row.springs.len() {
        if i_group == row.group.len() && i_contiguous_damaged == 0 {
            return 1;
        } else if i_group == row.group.len().saturating_sub(1) && i_contiguous_damaged == row.group[i_group]
        {
            return 1;
        }
        return 0;
    }

    let cache_key = (i_springs, i_group, i_contiguous_damaged);
    if let Some(&val) = cache.get(&cache_key) {
        return val;
    }

    let mut res = 0;
    let char = row.springs.chars().nth(i_springs).unwrap();
    if char == '.' || char == '?' {
        if i_contiguous_damaged == 0 {
            res += count_arrangements_recursive(
                row,
                i_springs + 1,
                i_group,
                i_contiguous_damaged,
                cache,
            );
        } else if i_contiguous_damaged == row.group[i_group] {
            res += count_arrangements_recursive(row, i_springs + 1, i_group + 1, 0, cache);
        }
    }
    if char == '#' || char == '?' {
        if i_group < row.group.len() && i_contiguous_damaged < row.group[i_group] {
            res += count_arrangements_recursive(
                row,
                i_springs + 1,
                i_group,
                i_contiguous_damaged + 1,
                cache,
            );
        }
    }

    cache.insert(cache_key, res);
    res
}

fn count_arrangements(row: &Row) -> usize {
    let mut cache = HashMap::new();
    count_arrangements_recursive(row, 0, 0, 0, &mut cache)
}

fn unfold_row(row: &Row, unfolding_factor: usize) -> Row {
    let mut springs = String::new();
    let mut group = Vec::new();

    for i in 0..unfolding_factor {
        springs.push_str(&row.springs);
        if i < unfolding_factor - 1 {
            springs.push('?');
        }
        group.extend_from_slice(&row.group);
    }

    Row { springs, group }
}

fn solve(input: &str) -> usize {
    let rows = parse_input(input);
    rows.iter().map(|row| count_arrangements(row)).sum()
}

fn read_file(filename: &str) -> io::Result<String> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut result = String::new();
    for line in reader.lines() {
        result.push_str(&line?);
        result.push('\n');
    }

    Ok(result.trim().to_string())
}

fn main() {
    match read_file("input.txt") {
        Ok(input) => {
            println!("{}", solve(&input));
        }
        Err(e) => println!("Error reading file: {}", e),
    }
}
