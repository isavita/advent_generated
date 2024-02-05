
use std::fs::File;
use std::io::{BufReader, prelude::*};

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);

    let mut count = 0;
    for line in reader.lines() {
        let pair = line.unwrap();
        let ranges: Vec<Vec<i32>> = pair
            .split(",")
            .map(|s| parse_range(s))
            .collect();

        let left = &ranges[0];
        let right = &ranges[1];

        if left[0] <= right[1] && left[1] >= right[0] {
            count += 1;
        }
    }

    println!("{}", count);
}

fn parse_range(s: &str) -> Vec<i32> {
    let split: Vec<&str> = s.split("-").collect();
    let start: i32 = split[0].parse().unwrap();
    let end: i32 = split[1].parse().unwrap();
    vec![start, end]
}
