
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut lines = input.lines();
    let initial_state: Vec<char> = lines.next().unwrap().split(": ").nth(1).unwrap().trim().chars().collect();
    let _ = lines.next(); // skip empty line

    let mut rules = vec![("", ""); 0];
    for line in lines {
        let parts: Vec<&str> = line.split(" => ").collect();
        rules.push((parts[0], parts[1]));
    }

    let mut state: Vec<char> = vec!['.'; initial_state.len() + 1000];
    for (i, c) in initial_state.iter().enumerate() {
        state[i + 500] = *c;
    }

    let mut offset = 500;
    for _ in 0..20 {
        let mut new_state = vec!['.'; state.len()];
        for i in 2..state.len() - 2 {
            let pattern: String = state[i - 2..=i + 2].iter().collect();
            for rule in &rules {
                if rule.0 == pattern {
                    new_state[i] = rule.1.chars().next().unwrap();
                    break;
                }
            }
        }
        state = new_state;
    }

    let sum: i32 = state.iter().enumerate().filter(|(_, &c)| c == '#').map(|(i, _)| i as i32 - offset).sum();
    println!("{}", sum);
}
