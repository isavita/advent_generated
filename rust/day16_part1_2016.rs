
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let disk_length = 272;
    
    let mut data = input.clone();
    while data.len() < disk_length {
        let a = data.clone();
        let b = a.chars().rev().map(|c| if c == '0' { '1' } else { '0' }).collect::<String>();
        data = format!("{}0{}", a, b);
    }
    
    let data = &data[..disk_length];

    let checksum = calculate_checksum(data);
    
    println!("{}", checksum);
}

fn calculate_checksum(data: &str) -> String {
    let mut checksum = data.to_string();
    
    while checksum.len() % 2 == 0 {
        let mut new_checksum = String::new();
        let chars: Vec<char> = checksum.chars().collect();
        
        for i in (0..chars.len()).step_by(2) {
            if chars[i] == chars[i + 1] {
                new_checksum.push('1');
            } else {
                new_checksum.push('0');
            }
        }
        
        checksum = new_checksum;
    }
    
    checksum
}
