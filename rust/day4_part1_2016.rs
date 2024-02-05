use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let rooms: Vec<&str> = input.trim().split('\n').collect();

    let mut sum = 0;

    for room in rooms {
        let parts: Vec<&str> = room.split(|c| c == '-' || c == '[' || c == ']').collect();
        let name = parts[..parts.len()-3].join("");
        let id: i32 = parts[parts.len()-3].parse().unwrap();
        let checksum = parts[parts.len()-2];

        let mut freq = vec![0; 26];
        for c in name.chars() {
            if c != '-' {
                freq[c as usize - 'a' as usize] += 1;
            }
        }

        let mut count = 0;
        let mut valid = true;
        for _ in 0..5 {
            let max_freq = freq.iter().max().unwrap();
            let max_index = freq.iter().position(|&x| x == *max_freq).unwrap();
            freq[max_index] = 0;

            if checksum.chars().nth(count).unwrap() != (max_index as u8 + 'a' as u8) as char {
                valid = false;
                break;
            }

            count += 1;
        }

        if valid {
            sum += id;
        }
    }

    println!("{}", sum);
}