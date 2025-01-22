
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn parse_rules(lines: &mut std::io::Lines<std::io::BufReader<File>>) -> HashMap<u32, Vec<u32>> {
    let mut rules = HashMap::new();
    for line in lines.by_ref() {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }
        let parts: Vec<&str> = line.split('|').collect();
        let before: u32 = parts[0].parse().unwrap();
        let after: u32 = parts[1].parse().unwrap();
        rules.entry(before).or_insert_with(Vec::new).push(after);
    }
    rules
}

fn parse_updates(lines: &mut std::io::Lines<std::io::BufReader<File>>) -> Vec<Vec<u32>> {
    let mut updates = Vec::new();
    for line in lines.by_ref() {
        let line = line.unwrap();
        let pages: Vec<u32> = line
            .split(',')
            .map(|s| s.parse().unwrap())
            .collect();
        updates.push(pages);
    }
    updates
}

fn is_update_correct(update: &[u32], rules: &HashMap<u32, Vec<u32>>) -> bool {
    for (i, &page1) in update.iter().enumerate() {
        if let Some(afters) = rules.get(&page1) {
            for &page2 in afters {
                if update.contains(&page2) {
                    if let Some(index2) = update.iter().position(|&x| x == page2){
                        if index2 < i {
                            return false;
                        }
                    }
                }
            }
        }
    }
    true
}

fn order_update(update: &mut Vec<u32>, rules: &HashMap<u32, Vec<u32>>){
    let mut n = update.len();
    while n > 0 {
        let mut swapped = false;
        for i in 0..n-1{
            let page1 = update[i];
            let page2 = update[i+1];
             
            let mut swap = false;

            if let Some(afters) = rules.get(&page1){
                if afters.contains(&page2){
                    swap = true;
                }
            }

            if swap{
                update.swap(i, i+1);
                swapped = true;
                
            }
        }

        if !swapped {
            break;
        }
        n -= 1;
    }
}

fn get_middle_page(pages: &[u32]) -> u32 {
    let mid = pages.len() / 2;
    pages[mid]
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines();

    let rules = parse_rules(&mut lines);
    let updates = parse_updates(&mut lines);

    let mut correct_sum = 0;
    let mut incorrect_sum = 0;
    let mut incorrect_updates = Vec::new();

    for update in updates {
        let mut update_copy = update.clone();
        if is_update_correct(&update_copy, &rules) {
            correct_sum += get_middle_page(&update_copy);
        } else {
            incorrect_updates.push(update_copy);
        }
    }
     
    for mut update in incorrect_updates{
        order_update(&mut update, &rules);
         incorrect_sum += get_middle_page(&update);
    }


    println!("Sum of middle pages of correctly ordered updates: {}", correct_sum);
    println!("Sum of middle pages of incorrectly ordered updates: {}", incorrect_sum);
    Ok(())
}
