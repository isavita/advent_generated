
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut initialState = String::new();
    let mut rules: HashMap<String, char> = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        if line.contains("initial state") {
            initialState = line.split(": ").last().unwrap().to_string();
        } else if line.contains("=>") {
            let parts: Vec<&str> = line.split(" => ").collect();
            rules.insert(parts[0].to_string(), parts[1].chars().next().unwrap());
        }
    }

    let mut state: HashMap<i64, char> = HashMap::new();
    for (i, c) in initialState.chars().enumerate() {
        if c == '#' {
            state.insert(i as i64, '#');
        }
    }

    let mut previous_pattern = String::new();
    let mut previous_sum = 0;
    let mut offset = 0;
    for generation in 0..50000000000 {
        let mut new_state: HashMap<i64, char> = HashMap::new();
        let (min_pot, max_pot) = min_max_keys(&state);
        for i in min_pot - 2..=max_pot + 2 {
            let mut pattern = String::new();
            for j in i - 2..=i + 2 {
                if let Some('#') = state.get(&j) {
                    pattern.push('#');
                } else {
                    pattern.push('.');
                }
            }
            if let Some('#') = rules.get(&pattern) {
                new_state.insert(i, '#');
            }
        }
        state = new_state;

        let (current_pattern, current_sum) = state_pattern(&state);
        if current_pattern == previous_pattern {
            offset = current_sum - previous_sum;
            let remaining_generations = 50000000000 - generation - 1;
            let final_sum = current_sum + offset * remaining_generations;
            println!("{}", final_sum);
            return Ok(());
        }
        previous_pattern = current_pattern;
        previous_sum = current_sum;
    }

    Ok(())
}

fn min_max_keys(m: &HashMap<i64, char>) -> (i64, i64) {
    let mut first = true;
    let mut min_key = 0;
    let mut max_key = 0;
    for &k in m.keys() {
        if first {
            min_key = k;
            max_key = k;
            first = false;
        } else {
            if k < min_key {
                min_key = k;
            }
            if k > max_key {
                max_key = k;
            }
        }
    }
    (min_key, max_key)
}

fn state_pattern(m: &HashMap<i64, char>) -> (String, i64) {
    let (min_pot, max_pot) = min_max_keys(m);
    let mut pattern = String::new();
    let mut sum = 0;
    for i in min_pot..=max_pot {
        if let Some('#') = m.get(&i) {
            pattern.push('#');
            sum += i;
        } else {
            pattern.push('.');
        }
    }
    (pattern, sum)
}
