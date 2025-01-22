
use std::fs::File;
use std::io::{self, BufRead};

fn is_safe(levels: &[i32]) -> bool {
    if levels.len() <= 1 {
        return true;
    }

    let mut increasing = None;
    for i in 0..levels.len() - 1 {
        let diff = levels[i + 1] - levels[i];
        if diff == 0 || diff < -3 || diff > 3 {
            return false;
        }
        match increasing {
            None => increasing = Some(diff > 0),
            Some(inc) => {
                if (diff > 0) != inc {
                    return false;
                }
            }
        }
    }
    true
}

fn is_safe_with_dampener(levels: &[i32]) -> bool {
    if is_safe(levels) {
        return true;
    }

    for i in 0..levels.len() {
        let mut temp_levels = Vec::with_capacity(levels.len() - 1);
        for j in 0..levels.len() {
            if i != j {
                temp_levels.push(levels[j]);
            }
        }

        if is_safe(&temp_levels) {
            return true;
        }
    }
    false
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut safe_count_part1 = 0;
    let mut safe_count_part2 = 0;

    for line in reader.lines() {
        let line = line?;
        let levels: Vec<i32> = line
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();

        if is_safe(&levels) {
            safe_count_part1 += 1;
            safe_count_part2 += 1;
        } else if is_safe_with_dampener(&levels) {
            safe_count_part2 += 1;
        }
    }

    println!("Part 1: {}", safe_count_part1);
    println!("Part 2: {}", safe_count_part2);

    Ok(())
}
