
use std::fs::File;
use std::io::{BufReader, prelude::*};

fn abs(x: i32) -> i32 {
    if x < 0 {
        return -x;
    }
    x
}

fn max(a: i32, b: i32) -> i32 {
    if a > b {
        return a;
    }
    b
}

fn distance(x: i32, y: i32, z: i32) -> i32 {
    (abs(x) + abs(y) + abs(z)) / 2
}

fn main() {
    let file = File::open("input.txt").expect("File not found");
    let reader = BufReader::new(file);

    let mut input = String::new();
    for line in reader.lines() {
        input = line.unwrap();
    }

    let directions: Vec<&str> = input.split(",").collect();

    let mut x = 0;
    let mut y = 0;
    let mut z = 0;
    let mut max_distance = 0;

    for dir in directions {
        match dir {
            "n" => {
                y += 1;
                z -= 1;
            }
            "ne" => {
                x += 1;
                z -= 1;
            }
            "se" => {
                x += 1;
                y -= 1;
            }
            "s" => {
                y -= 1;
                z += 1;
            }
            "sw" => {
                x -= 1;
                z += 1;
            }
            "nw" => {
                x -= 1;
                y += 1;
            }
            _ => {}
        }

        let cur_distance = distance(x, y, z);
        max_distance = max(max_distance, cur_distance);
    }

    println!("{}", distance(x, y, z));
}
