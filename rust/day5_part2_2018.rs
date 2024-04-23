use std::fs;

fn react_polymer(polymer: &str) -> String {
    let mut stack: Vec<char> = Vec::new();
    for c in polymer.chars() {
        if let Some(top) = stack.last() {
            if c.to_ascii_lowercase() == top.to_ascii_lowercase() && c != *top {
                stack.pop();
            } else {
                stack.push(c);
            }
        } else {
            stack.push(c);
        }
    }
    stack.into_iter().collect()
}

fn main() -> std::io::Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let reacted_polymer = react_polymer(&input);
    println!("Part 1: {}", reacted_polymer.len());

    let mut min_length = reacted_polymer.len();
    for c in 'a'..='z' {
        let filtered_polymer: String = input.chars().filter(|&x| x.to_ascii_lowercase() != c).collect();
        let reacted_polymer = react_polymer(&filtered_polymer);
        min_length = min_length.min(reacted_polymer.len());
    }
    println!("Part 2: {}", min_length);

    Ok(())
}