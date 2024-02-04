
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let ips: Vec<&str> = input.trim().split('\n').collect();
    
    let mut count = 0;
    for ip in ips {
        let mut valid = true;
        let mut in_hypernet = false;
        
        let mut abba_outside = false;
        let mut abba_inside = false;
        
        let mut chars: Vec<char> = ip.chars().collect();
        
        for i in 0..chars.len() - 3 {
            if chars[i] == '[' {
                in_hypernet = true;
            } else if chars[i] == ']' {
                in_hypernet = false;
            } else if chars[i] == chars[i + 3] && chars[i + 1] == chars[i + 2] && chars[i] != chars[i + 1] {
                if in_hypernet {
                    abba_inside = true;
                } else {
                    abba_outside = true;
                }
            }
        }
        
        if abba_outside && !abba_inside {
            count += 1;
        }
    }
    
    println!("{}", count);
}
