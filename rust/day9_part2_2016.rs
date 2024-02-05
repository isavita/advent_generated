
use std::fs;

fn decompressed_length_v1(input: &str) -> usize {
    let mut length = 0;
    let mut chars = input.chars().collect::<Vec<char>>();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '(' {
            let marker_start = i;
            while chars[i] != ')' {
                i += 1;
            }
            let marker = chars[marker_start + 1..i].iter().collect::<String>();
            let parts: Vec<&str> = marker.split('x').collect();
            let repeat_len = parts[0].parse::<usize>().unwrap();
            let repeat_times = parts[1].parse::<usize>().unwrap();

            i += 1;

            let repeat_section = chars[i..i + repeat_len].iter().collect::<String>();
            length += repeat_section.len() * repeat_times;

            i += repeat_len;
        } else {
            length += 1;
            i += 1;
        }
    }

    length
}

fn decompressed_length_v2(input: &str) -> usize {
    let mut length = 0;
    let mut chars = input.chars().collect::<Vec<char>>();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '(' {
            let marker_start = i;
            while chars[i] != ')' {
                i += 1;
            }
            let marker = chars[marker_start + 1..i].iter().collect::<String>();
            let parts: Vec<&str> = marker.split('x').collect();
            let repeat_len = parts[0].parse::<usize>().unwrap();
            let repeat_times = parts[1].parse::<usize>().unwrap();

            i += 1;

            let repeat_section = chars[i..i + repeat_len].iter().collect::<String>();
            length += decompressed_length_v2(&repeat_section) * repeat_times;

            i += repeat_len;
        } else {
            length += 1;
            i += 1;
        }
    }

    length
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Error reading input file");
    
    let decompressed_length_part1 = decompressed_length_v1(&input.replace(" ", "").replace("\n", ""));
    let decompressed_length_part2 = decompressed_length_v2(&input.replace(" ", "").replace("\n", ""));

    println!("{}", decompressed_length_part1);
    println!("{}", decompressed_length_part2);
}
