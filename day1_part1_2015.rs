
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    
    let floor: i32 = input.chars().map(|c| if c == '(' { 1 } else { -1 }).sum();
    
    println!("{}", floor);
}
