
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut lines = Vec::new();

    for line in reader.lines() {
        lines.push(line.unwrap());
    }

    for i in 0..lines.len() - 1 {
        for j in i + 1..lines.len() {
            let mut diff = 0;
            for k in 0..lines[i].len() {
                if lines[i].chars().nth(k) != lines[j].chars().nth(k) {
                    diff += 1;
                    if diff > 1 {
                        break;
                    }
                }
            }
            if diff == 1 {
                let mut common = String::new();
                for k in 0..lines[i].len() {
                    if lines[i].chars().nth(k) == lines[j].chars().nth(k) {
                        common.push(lines[i].chars().nth(k).unwrap());
                    }
                }
                println!("{}", common);
                return;
            }
        }
    }
}
