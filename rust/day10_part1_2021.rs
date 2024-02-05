
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    
    let mut total_score = 0;

    for line in input.lines() {
        let mut stack: Vec<char> = Vec::new();
        let mut score = 0;
        let mut found = false;

        for c in line.chars() {
            match c {
                '(' | '[' | '{' | '<' => stack.push(c),
                ')' => {
                    if let Some(last) = stack.pop() {
                        if last != '(' {
                            score = 3;
                            found = true;
                            break;
                        }
                    } else {
                        score = 3;
                        found = true;
                        break;
                    }
                },
                ']' => {
                    if let Some(last) = stack.pop() {
                        if last != '[' {
                            score = 57;
                            found = true;
                            break;
                        }
                    } else {
                        score = 57;
                        found = true;
                        break;
                    }
                },
                '}' => {
                    if let Some(last) = stack.pop() {
                        if last != '{' {
                            score = 1197;
                            found = true;
                            break;
                        }
                    } else {
                        score = 1197;
                        found = true;
                        break;
                    }
                },
                '>' => {
                    if let Some(last) = stack.pop() {
                        if last != '<' {
                            score = 25137;
                            found = true;
                            break;
                        }
                    } else {
                        score = 25137;
                        found = true;
                        break;
                    }
                },
                _ => (),
            }
        }

        if found {
            total_score += score;
        }
    }

    println!("{}", total_score);
}
