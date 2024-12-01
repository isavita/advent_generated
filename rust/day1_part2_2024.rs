use std::fs::read_to_string;
use std::collections::HashMap;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = read_to_string("input.txt")?;
    
    let mut left_list = Vec::new();
    let mut right_counts = HashMap::new();
    
    for line in content.lines() {
        if line.is_empty() { continue; }
        let numbers: Vec<i32> = line.split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();
            
        if let (Some(&left), Some(&right)) = (numbers.first(), numbers.last()) {
            left_list.push(left);
            *right_counts.entry(right).or_insert(0) += 1;
        }
    }
    
    let similarity_score: i64 = left_list.iter()
        .map(|&num| (num as i64) * (*right_counts.get(&num).unwrap_or(&0) as i64))
        .sum();
        
    println!("Similarity score: {}", similarity_score);
    Ok(())
}
