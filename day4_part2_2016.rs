
use std::fs::File;
use std::io::{BufReader, prelude::*};
use std::collections::HashMap;

fn main() {
    let file = File::open("input.txt").expect("Failed to open file");
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line.unwrap();
        if is_real_room(&line) {
            let decrypted_name = decrypt_name(&line);
            if decrypted_name.contains("northpole object") {
                println!("{}", get_sector_id(&line));
                break;
            }
        }
    }
}

fn is_real_room(room: &str) -> bool {
    let parts: Vec<&str> = room.split('[').collect();
    let checksum = parts[1].trim_matches(']');
    let encrypted_name: Vec<&str> = parts[0].split('-').collect();
    let encrypted_name = &encrypted_name[..encrypted_name.len()-1];

    let mut letter_counts = HashMap::new();
    for part in encrypted_name {
        for letter in part.chars() {
            *letter_counts.entry(letter).or_insert(0) += 1;
        }
    }

    let mut counts: Vec<(char, i32)> = letter_counts.into_iter().collect();
    counts.sort_by(|a, b| {
        if a.1 == b.1 {
            a.0.cmp(&b.0)
        } else {
            b.1.cmp(&a.1)
        }
    });

    for (i, ch) in checksum.chars().enumerate() {
        if ch != counts[i].0 {
            return false;
        }
    }

    true
}

fn get_sector_id(room: &str) -> i32 {
    let parts: Vec<&str> = room.split('-').collect();
    let sector_id_part = parts[parts.len()-1];
    sector_id_part.split('[').next().unwrap().parse().unwrap()
}

fn decrypt_name(room: &str) -> String {
    let parts: Vec<&str> = room.split('-').collect();
    let sector_id_part = parts[parts.len()-1];
    let sector_id: i32 = sector_id_part.split('[').next().unwrap().parse().unwrap();
    let mut decrypted_name = String::new();

    for part in &parts[..parts.len()-1] {
        for letter in part.chars() {
            if letter == '-' {
                decrypted_name.push(' ');
            } else {
                let shifted_letter = ((letter as i32 - 'a' as i32 + sector_id) % 26 + 'a' as i32) as u8 as char;
                decrypted_name.push(shifted_letter);
            }
        }
        decrypted_name.push(' ');
    }

    decrypted_name.trim().to_string()
}
