use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let lines: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();

    let mut count = 0;
    for i in 1..lines.len() {
        if lines[i] > lines[i - 1] {
            count += 1;
        }
    }

    println!("{}", count);
}