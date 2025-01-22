
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn is_safe(levels: &[i32]) -> bool {
    if levels.len() < 2 {
        return true; 
    }

    let mut increasing = None;
    for i in 0..levels.len() - 1 {
        let diff = levels[i + 1] - levels[i];
        if diff == 0 {
            return false;
        }
        if let Some(inc) = increasing {
            if (diff > 0) != inc {
                return false;
            }
        } else {
            increasing = Some(diff > 0);
        }
        if diff.abs() < 1 || diff.abs() > 3 {
            return false;
        }
    }
    true
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    
    let mut safe_count = 0;

    for line in reader.lines() {
        let line = line?;
        let levels: Vec<i32> = line
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();
        if is_safe(&levels) {
            safe_count += 1;
        }
    }
    println!("{}", safe_count);
    Ok(())
}
