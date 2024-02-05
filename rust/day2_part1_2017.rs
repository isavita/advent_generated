use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut checksum = 0;

    for line in contents.lines() {
        let numbers: Vec<i32> = line.split_whitespace()
            .map(|num| num.parse().unwrap())
            .collect();

        let max = numbers.iter().max().unwrap();
        let min = numbers.iter().min().unwrap();

        checksum += max - min;
    }

    println!("{}", checksum);
}