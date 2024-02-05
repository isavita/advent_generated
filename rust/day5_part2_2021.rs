
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn abs(x: i32) -> i32 {
    if x < 0 {
        return -x;
    }
    x
}

fn sign(x: i32) -> i32 {
    if x > 0 {
        return 1;
    } else if x < 0 {
        return -1;
    }
    0
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let lines: Vec<[i32; 4]> = io::BufReader::new(file)
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let parts: Vec<&str> = line.split(" -> ").collect();
            let start: Vec<&str> = parts[0].split(",").collect();
            let end: Vec<&str> = parts[1].split(",").collect();
            let x1 = start[0].parse().unwrap();
            let y1 = start[1].parse().unwrap();
            let x2 = end[0].parse().unwrap();
            let y2 = end[1].parse().unwrap();
            [x1, y1, x2, y2]
        })
        .collect();

    let mut overlaps = std::collections::HashMap::new();

    for line in lines {
        let x1 = line[0];
        let y1 = line[1];
        let x2 = line[2];
        let y2 = line[3];

        let x_step = sign(x2 - x1);
        let y_step = sign(y2 - y1);
        let steps = abs(x2 - x1) + 1;
        let steps = if abs(y2 - y1) > abs(x2 - x1) {
            abs(y2 - y1) + 1
        } else {
            steps
        };

        for i in 0..steps {
            let point = [x1 + i * x_step, y1 + i * y_step];
            *overlaps.entry(point).or_insert(0) += 1;
        }
    }

    let mut count = 0;
    for &v in overlaps.values() {
        if v > 1 {
            count += 1;
        }
    }

    println!("{}", count);
}
