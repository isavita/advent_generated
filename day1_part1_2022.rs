use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let elves: Vec<i32> = input.trim().split("\n\n").map(|group| group.lines().map(|line| line.parse::<i32>().unwrap()).sum()).collect();
    
    let max_calories = elves.iter().max().unwrap();
    
    println!("{}", max_calories);
}