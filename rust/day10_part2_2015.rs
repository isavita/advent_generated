
use std::fs;

fn main() {
    let initial_sequence = read_input("input.txt").unwrap();
    let result = look_and_say(&initial_sequence, 50);
    println!("{}", result.len());
}

fn read_input(filename: &str) -> Result<String, std::io::Error> {
    let content = fs::read_to_string(filename)?;
    Ok(content)
}

fn look_and_say(sequence: &str, iterations: u32) -> String {
    let mut sequence = sequence.to_string();
    for _ in 0..iterations {
        sequence = next_sequence(&sequence);
    }
    sequence
}

fn next_sequence(sequence: &str) -> String {
    let mut result = String::new();
    let mut chars = sequence.chars();
    let mut count = 1;
    let mut digit = chars.next().unwrap();

    for c in chars {
        if c == digit {
            count += 1;
        } else {
            result.push_str(&count.to_string());
            result.push(digit);
            count = 1;
            digit = c;
        }
    }

    result.push_str(&count.to_string());
    result.push(digit);

    result
}
