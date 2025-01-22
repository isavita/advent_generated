
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let mut lines = read_lines("input.txt")?;
    let mut rules = Vec::new();

    // Read rules until we hit an empty line
    while let Some(line) = lines.next() {
        let line = line?;
        if line.is_empty() {
            break;
        }
        let parts: Vec<&str> = line.split('|').collect();
        if parts.len() == 2 {
            let before: u32 = parts[0].parse().unwrap();
            let after: u32 = parts[1].parse().unwrap();
            rules.push((before, after));
        }
    }

    let mut total_middle_pages = 0;
    for line in lines {
        let line = line?;
        let pages: Vec<u32> = line
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect();

        if is_valid_update(&pages, &rules) {
            let middle_index = pages.len() / 2;
             total_middle_pages += pages[middle_index];
        }
    }
    println!("{}", total_middle_pages);
    Ok(())
}

fn is_valid_update(update: &[u32], rules: &[(u32, u32)]) -> bool {
    let page_set: HashSet<u32> = update.iter().cloned().collect();
    for &(before, after) in rules {
        if page_set.contains(&before) && page_set.contains(&after) {
            if let (Some(index_before), Some(index_after)) = (
                update.iter().position(|&p| p == before),
                update.iter().position(|&p| p == after),
            ) {
                if index_before >= index_after {
                     return false;
                }
            }
        }
    }
    true
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
