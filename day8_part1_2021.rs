
use std::fs::File;
use std::io::{BufReader, BufRead};

fn main() {
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut count = 0;
    for line in reader.lines() {
        let line = line.expect("Unable to read line");
        let parts: Vec<&str> = line.split(" | ").collect();
        let output = parts[1];
        for digit in output.split(" ") {
            match digit.len() {
                2 | 4 | 3 | 7 => count += 1,
                _ => (),
            }
        }
    }

    println!("{}", count);
}
