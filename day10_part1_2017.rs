
use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").expect("File reading error");
    let lengths_str: Vec<&str> = contents.trim().split(",").collect();
    let mut lengths: Vec<usize> = Vec::new();
    for l in lengths_str {
        let n: usize = l.parse().unwrap();
        lengths.push(n);
    }

    let mut list: Vec<usize> = (0..256).collect();
    let mut current_position = 0;
    let mut skip_size = 0;

    for &length in lengths.iter() {
        for i in 0..length/2 {
            let start = (current_position + i) % 256;
            let end = (current_position + length - 1 - i) % 256;
            list.swap(start, end);
        }

        current_position = (current_position + length + skip_size) % 256;
        skip_size += 1;
    }

    let result = list[0] * list[1];
    println!("{}", result);
}
