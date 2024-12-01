use std::fs::read_to_string;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = read_to_string("input.txt")?;
    
    let mut left_list = Vec::new();
    let mut right_list = Vec::new();
    
    for line in content.lines() {
        if line.is_empty() { continue; }
        let numbers: Vec<i32> = line.split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();
            
        if let (Some(&left), Some(&right)) = (numbers.first(), numbers.last()) {
            left_list.push(left);
            right_list.push(right);
        }
    }
    
    left_list.sort_unstable();
    right_list.sort_unstable();
    
    let total_distance: i64 = left_list.iter()
        .zip(right_list.iter())
        .map(|(a, b)| (a - b).abs() as i64)
        .sum();
        
    println!("Total distance: {}", total_distance);
    Ok(())
}
