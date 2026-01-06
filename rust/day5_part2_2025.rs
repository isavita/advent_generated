
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let mut ranges: Vec<(u64, u64)> = io::BufReader::new(file)
        .lines()
        .filter_map(|l| {
            let line = l.ok()?.trim().to_string();
            let mut sp = line.splitn(2, '-');
            let a = sp.next()?.parse().ok()?;
            let b = sp.next()?.parse().ok()?;
            Some(if a <= b { (a, b) } else { (b, a) })
        })
        .collect();

    if ranges.is_empty() {
        println!("Total fresh IDs: 0");
        return Ok(());
    }

    ranges.sort_unstable_by_key(|&(s, _)| s);
    let (mut cur_min, mut cur_max) = ranges[0];
    let mut total: u64 = 0;

    for &(min, max) in &ranges[1..] {
        if min <= cur_max {
            cur_max = cur_max.max(max);
        } else {
            total += cur_max - cur_min + 1;
            cur_min = min;
            cur_max = max;
        }
    }
    total += cur_max - cur_min + 1;

    println!("Total fresh IDs: {}", total);
    Ok(())
}
