
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    
    let groups: Vec<&str> = input.trim().split("\n\n").collect();
    
    let total_count: usize = groups.iter().map(|group| {
        let mut answers = [false; 26];
        let people: Vec<&str> = group.split("\n").collect();
        
        for person in people {
            for c in person.chars() {
                answers[c as usize - 'a' as usize] = true;
            }
        }
        
        answers.iter().filter(|&&x| x).count()
    }).sum();
    
    println!("{}", total_count);
}
