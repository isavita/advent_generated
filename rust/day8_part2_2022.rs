
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Read};

fn main() {
    let mut grid = HashMap::new();
    let mut s = String::new();
    let mut y = 0;

    let file = File::open("input.txt").expect("Unable to open file");
    let mut reader = BufReader::new(file);
    reader.read_to_string(&mut s).expect("Unable to read file");

    for line in s.lines() {
        for (x, b) in line.bytes().enumerate() {
            grid.insert((x as i32, y), (b - b'0') as i32);
        }
        y += 1;
    }

    let neighbors4 = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    let mut max_score = 0;

    for (&p, &value) in &grid {
        let mut score = 1;
        for &n in &neighbors4 {
            let mut next = p;
            let mut view = 0;
            loop {
                next = (next.0 + n.0, next.1 + n.1);
                if let Some(&v) = grid.get(&next) {
                    view += 1;
                    if v >= value {
                        score *= view;
                        break;
                    }
                } else {
                    score *= view;
                    break;
                }
            }
        }
        if score > max_score {
            max_score = score;
        }
    }

    println!("{}", max_score);
}
