
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines();

    let available_line = lines.next().unwrap()?;
    let available_patterns: HashSet<String> = available_line
        .split(',')
        .map(|s| s.trim().to_string())
        .collect();

    lines.next();

    let mut count = 0;
    for line in lines {
        let design = line?;
        if can_make(&design, &available_patterns) {
            count += 1;
        }
    }
    println!("{}", count);
    Ok(())
}

fn can_make(design: &str, patterns: &HashSet<String>) -> bool {
    let n = design.len();
    let mut dp = vec![false; n + 1];
    dp[0] = true;

    for i in 1..=n {
        for p in patterns {
            let lp = p.len();
            if i >= lp && dp[i - lp] && &design[i - lp..i] == p {
                dp[i] = true;
                break;
            }
        }
    }
    dp[n]
}
