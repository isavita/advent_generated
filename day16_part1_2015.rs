
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let mfcsam = vec![
        ("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3),
        ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3),
        ("cars", 2), ("perfumes", 1),
    ];

    let file = File::open("input.txt").expect("Error opening file");
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split_whitespace().collect();
        let sue_number = parts[1].trim_end_matches(':');

        let mut matches = true;
        for i in (2..parts.len()).step_by(2) {
            let item = parts[i].trim_end_matches(':');
            let count: i32 = parts[i + 1].trim_end_matches(',').parse().unwrap();
            if let Some(&(_, value)) = mfcsam.iter().find(|&&(key, _)| key == item) {
                if value != count {
                    matches = false;
                    break;
                }
            }
        }

        if matches {
            println!("{}", sue_number);
            break;
        }
    }
}
