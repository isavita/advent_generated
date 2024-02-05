use std::fs;

fn react(polymer: &str) -> String {
    for i in 0..polymer.len()-1 {
        if polymer.as_bytes()[i] != polymer.as_bytes()[i+1] &&
            (polymer.as_bytes()[i] + 32 == polymer.as_bytes()[i+1] ||
                polymer.as_bytes()[i] - 32 == polymer.as_bytes()[i+1]) {
            return react(&format!("{}{}", &polymer[..i], &polymer[i+2..]));
        }
    }
    polymer.to_string()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let polymer = input.trim();

    let result = react(polymer);
    println!("{}", result.len());
}