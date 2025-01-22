
use std::fs;
use std::cmp;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let ans = firewall(&input);
    println!("{}", ans);
}

fn firewall(input: &str) -> u32 {
    let mut all_blocked_ranges: Vec<[u32; 2]> = input
        .lines()
        .map(|line| {
            let parts: Vec<&str> = line.split('-').collect();
            [parts[0].parse().unwrap(), parts[1].parse().unwrap()]
        })
        .collect();

    all_blocked_ranges.sort_unstable();

    let mut merged: Vec<[u32; 2]> = Vec::with_capacity(all_blocked_ranges.len());
    for r in all_blocked_ranges {
        if let Some(last) = merged.last_mut() {
            if last[1] >= r[0].saturating_sub(1) {
                last[1] = cmp::max(last[1], r[1]);
                continue;
            }
        }
        merged.push(r);
    }


    if let Some(last) = merged.last(){
        if last[1] != u32::MAX{
          merged.push([u32::MAX,0]);
        }
    }


    let mut total_allowed = 0;
    for i in 1..merged.len(){
        total_allowed += merged[i][0].saturating_sub(merged[i-1][1]).saturating_sub(1);
    }
    total_allowed
}
