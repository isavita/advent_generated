use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let ids: Vec<&str> = input.trim().split('\n').collect();
    
    let mut twos = 0;
    let mut threes = 0;
    
    for id in ids {
        let mut counts = [0; 26];
        
        for c in id.chars() {
            let index = (c as u8 - b'a') as usize;
            counts[index] += 1;
        }
        
        if counts.contains(&2) {
            twos += 1;
        }
        
        if counts.contains(&3) {
            threes += 1;
        }
    }
    
    println!("{}", twos * threes);
}