
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let mut password = next_password(&input);
    password = next_password(&password);
    println!("{}", password);
}

fn next_password(password: &str) -> String {
    let mut password = password.chars().map(|c| c as u8).collect::<Vec<u8>>();
    loop {
        increment_password(&mut password);
        if is_valid_password(&password) {
            break;
        }
    }
    password.iter().map(|&c| c as char).collect()
}

fn increment_password(password: &mut Vec<u8>) {
    let mut i = password.len() - 1;
    loop {
        password[i] = (password[i] - b'a' + 1) % 26 + b'a';
        if password[i] != b'a' {
            break;
        }
        if i == 0 {
            password.insert(0, b'a');
        }
        i -= 1;
    }
}

fn is_valid_password(password: &Vec<u8>) -> bool {
    let password_str = password.iter().map(|&c| c as char).collect::<String>();
    if password_str.contains('i') || password_str.contains('o') || password_str.contains('l') {
        return false;
    }
    let mut straight = false;
    for i in 0..password.len() - 2 {
        if password[i] + 1 == password[i + 1] && password[i + 1] + 1 == password[i + 2] {
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
        if password[i] == password[i + 1] {
            pairs += 1;
            i += 2;
        } else {
            i += 1;
        }
    }
    pairs >= 2
}
