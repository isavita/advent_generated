use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read input file");
    let mut happiness_map: HashMap<(&str, &str), i32> = HashMap::new();

    for line in input.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let person1 = parts[0];
        let person2 = parts[10].trim_matches('.');

        let happiness = if parts[2] == "gain" {
            parts[3].parse::<i32>().unwrap()
        } else {
            -parts[3].parse::<i32>().unwrap()
        };

        happiness_map.insert((person1, person2), happiness);
    }

    let mut people: Vec<&str> = happiness_map.keys().map(|&(p1, _)| p1).collect();
    people.sort();
    people.dedup();

    let mut max_happiness = 0;

    permute(&mut people, 0, &happiness_map, &mut max_happiness);

    println!("{}", max_happiness);
}

fn permute(people: &mut Vec<&str>, start: usize, happiness_map: &HashMap<(&str, &str), i32>, max_happiness: &mut i32) {
    if start == people.len() {
        let mut happiness = 0;

        for i in 0..people.len() {
            let person1 = people[i];
            let person2 = people[(i + 1) % people.len()];
            let person3 = people[(i + people.len() - 1) % people.len()];

            happiness += happiness_map[&(person1, person2)];
            happiness += happiness_map[&(person1, person3)];
        }

        if happiness > *max_happiness {
            *max_happiness = happiness;
        }
    } else {
        for i in start..people.len() {
            people.swap(start, i);
            permute(people, start + 1, happiness_map, max_happiness);
            people.swap(start, i);
        }
    }
}