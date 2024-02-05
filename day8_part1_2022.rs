use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let mut grid = HashMap::new();
    let mut visible = HashMap::new();
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);
    let mut y = 0;

    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        for (x, b) in line.bytes().enumerate() {
            grid.insert((x as i32, y), (b - b'0') as i32);
        }
        y += 1;
    }

    let neighbors4 = [(0, 1), (0, -1), (1, 0), (-1, 0)];

    for (&p, _) in &grid {
        for &n in &neighbors4 {
            let mut next = p;
            loop {
                next = (next.0 + n.0, next.1 + n.1);
                if grid.contains_key(&next) {
                    if *grid.get(&next).unwrap() >= *grid.get(&p).unwrap() {
                        break;
                    }
                } else {
                    visible.insert(p, ());
                    break;
                }
            }
        }
    }

    println!("{}", visible.len());
}