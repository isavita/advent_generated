
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);

    let mut numbers: Vec<Vec<i32>> = Vec::new();
    for line in reader.lines() {
        let sides: Vec<i32> = line.unwrap().split_whitespace().map(|s| s.parse().unwrap()).collect();
        numbers.push(sides);
    }

    let mut valid_triangles = 0;
    for i in 0..numbers[0].len() {
        for j in (0..numbers.len()).step_by(3) {
            if j + 2 < numbers.len() && is_valid_triangle(numbers[j][i], numbers[j + 1][i], numbers[j + 2][i]) {
                valid_triangles += 1;
            }
        }
    }

    println!("{}", valid_triangles);
}

fn is_valid_triangle(a: i32, b: i32, c: i32) -> bool {
    a + b > c && a + c > b && b + c > a
}
