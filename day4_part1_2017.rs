use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let valid_passphrases = input.lines().filter(|line| {
        let words: Vec<&str> = line.split_whitespace().collect();
        let mut unique_words = std::collections::HashSet::new();
        for word in words {
            if !unique_words.insert(word) {
                return false;
            }
        }
        true
    }).count();

    println!("{}", valid_passphrases);
}