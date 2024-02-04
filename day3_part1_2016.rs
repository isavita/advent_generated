
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut count = 0;

    for line in input.lines() {
        let sides: Vec<i32> = line.split_whitespace().map(|x| x.parse().unwrap()).collect();
        if sides[0] + sides[1] > sides[2] && sides[1] + sides[2] > sides[0] && sides[0] + sides[2] > sides[1] {
            count += 1;
        }
    }

    println!("{}", count);
}
