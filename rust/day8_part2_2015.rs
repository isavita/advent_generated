use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut code_chars = 0;
    let mut memory_chars = 0;
    let mut encoded_chars = 0;

    for line in input.lines() {
        code_chars += line.len();
        memory_chars += line[1..line.len()-1].replace("\\\"", "a").replace("\\\\", "a").replace("\\x", "a").len();
        encoded_chars += line.len() + 2 + line.chars().filter(|&c| c == '\\' || c == '"').count();
    }

    println!("{}", code_chars - memory_chars);
    println!("{}", encoded_chars - code_chars);
}