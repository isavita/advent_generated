use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let passphrases: Vec<&str> = input.trim().split('\n').collect();

    let mut valid_count = 0;
    let mut valid_anagram_count = 0;

    for passphrase in passphrases {
        let words: Vec<&str> = passphrase.split_whitespace().collect();
        let mut word_set = std::collections::HashSet::new();
        let mut anagram_set = std::collections::HashSet::new();
        let mut is_valid = true;
        let mut is_valid_anagram = true;

        for word in words {
            let mut sorted_word = word.chars().collect::<Vec<char>>();
            sorted_word.sort();
            let sorted_word_str: String = sorted_word.into_iter().collect();

            if !word_set.insert(word) {
                is_valid = false;
            }

            if !anagram_set.insert(sorted_word_str) {
                is_valid_anagram = false;
            }
        }

        if is_valid {
            valid_count += 1;
        }

        if is_valid_anagram {
            valid_anagram_count += 1;
        }
    }

    println!("{}", valid_count);
    println!("{}", valid_anagram_count);

    std::process::exit(0);
}