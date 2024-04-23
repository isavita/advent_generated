use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut scores: Vec<u64> = Vec::new(); // Use u64 to avoid overflow

    for line in reader.lines() {
        let line = line.unwrap();
        let (score, incomplete) = check_and_complete_line(&line);
        if incomplete {
            scores.push(score);
        }
    }

    scores.sort_unstable();
    let middle_score = scores[scores.len() / 2];
    println!("{}", middle_score);
}

fn check_and_complete_line(line: &str) -> (u64, bool) {
    let pairings = HashMap::from([(')', '('), (']', '['), ('}', '{'), ('>', '<')]);
    let score_values = HashMap::from([(')', 1u64), (']', 2), ('}', 3), ('>', 4)]);
    let opening = "([{<";
    let closing = ")]}>";
    let mut stack: Vec<char> = Vec::new();

    for char in line.chars() {
        if opening.contains(char) {
            stack.push(char);
        } else if closing.contains(char) {
            if stack.is_empty() || stack.last().unwrap() != &pairings[&char] {
                return (0, false); // corrupted line
            }
            stack.pop(); // pop from stack
        }
    }

    if stack.is_empty() {
        return (0, false); // line is not incomplete
    }

    // Calculate score for incomplete line
    let mut score: u64 = 0;
    for char in stack.iter().rev() {
        score = score * 5;
        score += score_values[&get_closing_char(*char)];
    }
    (score, true)
}

fn get_closing_char(opening_char: char) -> char {
    match opening_char {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => ' ',
    }
}