
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut jumps: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();
    
    let mut steps = 0;
    let mut current_index = 0;
    
    while current_index >= 0 && current_index < jumps.len() as i32 {
        let jump = jumps[current_index as usize];
        
        if jump >= 3 {
            jumps[current_index as usize] -= 1;
        } else {
            jumps[current_index as usize] += 1;
        }
        
        current_index += jump;
        steps += 1;
    }
    
    println!("{}", steps);
}
