
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut part1_count = 0;
    let mut part2_sum = 0;

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(" | ").collect();
        let patterns: Vec<&str> = parts[0].split(' ').collect();
        let output: Vec<&str> = parts[1].split(' ').collect();

        // Part 1 logic
        for digit in &output {
            match digit.len() {
                2 | 3 | 4 | 7 => part1_count += 1,
                _ => (),
            }
        }


        // Part 2 logic
        let mapping = deduce_mapping(&patterns);
        let decoded_output = decode_output(&output, &mapping);
        part2_sum += decoded_output;
    }

    println!("Part 1: {}", part1_count);
    println!("Part 2: {}", part2_sum);

    Ok(())
}


fn deduce_mapping(patterns: &Vec<&str>) -> HashMap<String, u32> {
    let mut mapping: HashMap<String, u32> = HashMap::new();
    let mut segments: HashMap<u32, String> = HashMap::new();

    // Find 1, 4, 7, 8
    for pattern in patterns {
        match pattern.len() {
            2 => { segments.insert(1, pattern.chars().collect::<String>()); },
            3 => { segments.insert(7, pattern.chars().collect::<String>()); },
            4 => { segments.insert(4, pattern.chars().collect::<String>()); },
            7 => { segments.insert(8, pattern.chars().collect::<String>()); },
            _ => {},
        }
    }


    // Deduce 0, 2, 3, 5, 6, 9
    for pattern in patterns {
        if pattern.len() == 5 { // 2, 3, or 5
            if segments.get(&1).unwrap().chars().all(|c| pattern.contains(c)){
                segments.insert(3, pattern.chars().collect::<String>());
            } else if segments.get(&4).unwrap().chars().filter(|&c| pattern.contains(c)).count() == 3 {
                segments.insert(5, pattern.chars().collect::<String>());
            } else {
                segments.insert(2, pattern.chars().collect::<String>());
            }
            
        } else if pattern.len() == 6 { // 0, 6, or 9
            if !segments.get(&1).unwrap().chars().all(|c| pattern.contains(c)){
                segments.insert(6, pattern.chars().collect::<String>());
            } else if segments.get(&4).unwrap().chars().all(|c| pattern.contains(c)){
                segments.insert(9, pattern.chars().collect::<String>());
            } else {
                segments.insert(0, pattern.chars().collect::<String>());
            }
        }
    }

    // Construct the mapping
    for (digit, pattern) in &segments {
        let mut sorted_pattern = pattern.chars().collect::<Vec<char>>();
        sorted_pattern.sort();
        mapping.insert(sorted_pattern.iter().collect::<String>(), *digit);
    }


    mapping
}



fn decode_output(output: &Vec<&str>, mapping: &HashMap<String, u32>) -> u32 {
    let mut decoded_value = String::new();
    for digit in output {
        let mut sorted_digit = digit.chars().collect::<Vec<char>>();
        sorted_digit.sort();
        decoded_value.push_str(&mapping.get(&sorted_digit.iter().collect::<String>()).unwrap().to_string());
    }
    decoded_value.parse::<u32>().unwrap()
}
