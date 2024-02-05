
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let mut password = next_password(&input);
    while !is_valid(&password) {
        password = next_password(&password);
    }
    println!("{}", password);
}

fn next_password(password: &str) -> String {
    let mut chars: Vec<char> = password.chars().collect();
    let mut i = chars.len() - 1;
    while i >= 0 {
        if chars[i] != 'z' {
            chars[i] = std::char::from_u32(chars[i] as u32 + 1).unwrap();
            break;
        } else {
            chars[i] = 'a';
            i -= 1;
        }
    }
    chars.iter().collect()
}

fn is_valid(password: &str) -> bool {
    if password.contains('i') || password.contains('o') || password.contains('l') {
        return false;
    }

    let mut straight = false;
    for i in 0..password.len() - 2 {
        if password.as_bytes()[i] + 1 == password.as_bytes()[i + 1] &&
           password.as_bytes()[i + 1] + 1 == password.as_bytes()[i + 2] {
            straight = true;
            break;
        }
    }
    if !straight {
        return false;
    }

    let mut pairs = 0;
    let mut i = 0;
    while i < password.len() - 1 {
        if password.as_bytes()[i] == password.as_bytes()[i + 1] {
            pairs += 1;
            i += 2;
        } else {
            i += 1;
        }
    }
    pairs >= 2
}
