
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut ranges: Vec<(i64, i64)> = Vec::new();
    let mut ids: Vec<i64> = Vec::new();
    let mut reading_ranges = true;
    for line in reader.lines() {
        let line = line.unwrap();
        if reading_ranges {
            let t = line.trim();
            if t.is_empty() {
                reading_ranges = false;
                continue;
            }
            let parts: Vec<&str> = t.split('-').collect();
            let start: i64 = parts[0].parse().unwrap();
            let end: i64 = parts[1].parse().unwrap();
            ranges.push((start, end));
        } else {
            for token in line.split_whitespace() {
                ids.push(token.parse::<i64>().unwrap());
            }
        }
    }
    if ranges.is_empty() {
        println!("0");
        return;
    }
    ranges.sort_by_key(|r| r.0);
    let mut merged: Vec<(i64, i64)> = Vec::new();
    let mut cur = ranges[0];
    for &(s, e) in ranges.iter().skip(1) {
        if s <= cur.1 + 1 {
            if e > cur.1 {
                cur.1 = e;
            }
        } else {
            merged.push(cur);
            cur = (s, e);
        }
    }
    merged.push(cur);
    let mut count = 0;
    for id in ids {
        let mut lo = 0usize;
        let mut hi = merged.len();
        let mut found = false;
        while lo < hi {
            let mid = (lo + hi) / 2;
            let (s, e) = merged[mid];
            if id < s {
                hi = mid;
            } else if id > e {
                lo = mid + 1;
            } else {
                found = true;
                break;
            }
        }
        if found {
            count += 1;
        }
    }
    println!("{}", count);
}
