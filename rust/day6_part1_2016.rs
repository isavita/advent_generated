
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.trim().split('\n').collect();

    let mut message = String::new();

    for i in 0..lines[0].len() {
        let mut char_count = std::collections::HashMap::new();
        for line in &lines {
            let c = line.chars().nth(i).unwrap();
            *char_count.entry(c).or_insert(0) += 1;
        }
        let most_common_char = char_count.iter().max_by_key(|&(_, count)| count).unwrap().0;
        message.push(*most_common_char);
    }

    println!("{}", message);
}
