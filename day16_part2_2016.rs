
use std::fs;

fn dragon_curve(input: &str, disk_length: usize) -> String {
    let mut data = String::from(input);
    while data.len() < disk_length {
        let mut new_data = data.clone();
        new_data = new_data.chars().rev().map(|c| if c == '0' { '1' } else { '0' }).collect::<String>();
        data.push('0');
        data.push_str(&new_data);
    }
    data.chars().take(disk_length).collect()
}

fn checksum(data: &str) -> String {
    let mut checksum = String::from(data);
    while checksum.len() % 2 == 0 {
        let mut new_checksum = String::new();
        let chars: Vec<char> = checksum.chars().collect();
        for i in (0..chars.len()).step_by(2) {
            if chars[i] == chars[i + 1] {
                new_checksum.push('1');
            } else {
                new_checksum.push('0');
            }
        }
        checksum = new_checksum;
    }
    checksum
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();
    
    let disk_length_1 = 272;
    let disk_length_2 = 35651584;

    let data_1 = dragon_curve(&input, disk_length_1);
    let checksum_1 = checksum(&data_1);
    println!("{}", checksum_1);

    let data_2 = dragon_curve(&input, disk_length_2);
    let checksum_2 = checksum(&data_2);
    println!("{}", checksum_2);
}
