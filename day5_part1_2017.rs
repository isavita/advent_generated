
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut jumps: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();
    let mut index: i32 = 0;
    let mut steps = 0;

    while index >= 0 && (index as usize) < jumps.len() {
        let jump = jumps[index as usize];
        jumps[index as usize] += 1;
        index += jump;
        steps += 1;
    }

    println!("{}", steps);
}
