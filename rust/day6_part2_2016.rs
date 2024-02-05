
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.trim().split('\n').collect();
    
    let mut message1 = String::new();
    let mut message2 = String::new();
    
    let mut columns: Vec<Vec<char>> = vec![Vec::new(); lines[0].len()];
    
    for line in lines {
        for (i, c) in line.chars().enumerate() {
            columns[i].push(c);
        }
    }
    
    for col in columns {
        let mut counts = std::collections::HashMap::new();
        
        for &c in &col {
            *counts.entry(c).or_insert(0) += 1;
        }
        
        let most_common = *counts.iter().max_by_key(|&(_, count)| count).unwrap().0;
        let least_common = *counts.iter().min_by_key(|&(_, count)| count).unwrap().0;
        
        message1.push(most_common);
        message2.push(least_common);
    }
    
    println!("{}", message1);
    println!("{}", message2);
}
