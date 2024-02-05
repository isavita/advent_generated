
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn item_priority(item: char) -> i32 {
    if item >= 'a' && item <= 'z' {
        return (item as i32 - 'a' as i32) + 1;
    }
    return (item as i32 - 'A' as i32) + 27;
}

fn main() {
    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);

    let mut sum = 0;
    let mut group_line_counter = 0;
    let mut group_items: [HashMap<char, i32>; 3] = [HashMap::new(), HashMap::new(), HashMap::new()];

    for line in reader.lines() {
        let line = line.expect("Error reading line");
        let mut items_map = HashMap::new();
        for item in line.chars() {
            *items_map.entry(item).or_insert(0) += 1;
        }
        group_items[group_line_counter] = items_map;
        group_line_counter += 1;

        if group_line_counter == 3 {
            let mut common_items = HashMap::new();
            for (item, _) in &group_items[0] {
                if group_items[1].contains_key(item) && group_items[2].contains_key(item) {
                    common_items.insert(*item, 1);
                }
            }
            for (item, _) in &common_items {
                sum += item_priority(*item);
                break; // Since we need only one common item per group
            }
            group_line_counter = 0;
        }
    }

    println!("{}", sum);
}
