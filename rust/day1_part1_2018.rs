use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let changes: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();
    
    let result: i32 = changes.iter().sum();
    
    println!("{}", result);
}