
use std::fs::File;
use std::io::{BufReader, BufRead};

fn item_priority(item: char) -> i32 {
    if item >= 'a' && item <= 'z' {
        return (item as i32 - 'a' as i32 + 1);
    }
    return (item as i32 - 'A' as i32) + 27;
}

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);

    let mut sum = 0;
    for line in reader.lines() {
        let line = line.expect("Error reading line");
        let half = line.len() / 2;
        let (first_compartment, second_compartment) = line.split_at(half);

        let mut compartment_map = std::collections::HashMap::new();
        for item in first_compartment.chars() {
            *compartment_map.entry(item).or_insert(0) += 1;
        }
        for item in second_compartment.chars() {
            if compartment_map.contains_key(&item) {
                sum += item_priority(item);
                break;
            }
        }
    }

    println!("{}", sum);
}
