use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let res = aunt_sue(input);
    println!("{}", res);
}

fn aunt_sue(input: String) -> i32 {
    let target_sue = vec![
        ("children", 3),
        ("cats", 7),
        ("samoyeds", 2),
        ("pomeranians", 3),
        ("akitas", 0),
        ("vizslas", 0),
        ("goldfish", 5),
        ("trees", 3),
        ("cars", 2),
        ("perfumes", 1),
    ];

    for line in input.trim().split("\n") {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let sue_num: i32 = parts[1].trim_matches(':').parse().unwrap();
        let mut readings_map: std::collections::HashMap<&str, i32> = std::collections::HashMap::new();

        for i in 2..parts.len() {
            if i % 2 == 0 {
                let thing = parts[i].trim_matches(':');
                let amount: i32 = parts[i + 1].trim_matches(',').parse().unwrap();
                readings_map.insert(thing, amount);
            }
        }

        let mut all_rules_matched = true;
        for (thing, target_amount) in target_sue.iter() {
            if let Some(scan_count) = readings_map.get(thing) {
                if *thing == "cats" || *thing == "trees" {
                    if *scan_count <= *target_amount {
                        all_rules_matched = false;
                    }
                } else if *thing == "pomeranians" || *thing == "goldfish" {
                    if *scan_count >= *target_amount {
                        all_rules_matched = false;
                    }
                } else {
                    if scan_count != target_amount {
                        all_rules_matched = false;
                    }
                }
            }
        }

        if all_rules_matched {
            return sue_num;
        }
    }

    panic!("expect return from loop");
}