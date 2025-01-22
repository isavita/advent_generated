
use std::collections::HashMap;
use std::fs;

#[derive(Debug)]
struct Rule {
    write_value: u8,
    move_direction: i32,
    next_state: char,
}

#[derive(Debug)]
struct State {
    rules: [Rule; 2],
}

fn parse_input(input: &str) -> (char, usize, HashMap<char, State>) {
    let mut lines = input.lines();
    let start_state = lines
        .next()
        .unwrap()
        .split_whitespace()
        .last()
        .unwrap()
        .chars()
        .next()
        .unwrap();
    let steps = lines
        .next()
        .unwrap()
        .split_whitespace()
        .nth(5)
        .unwrap()
        .parse::<usize>()
        .unwrap();

    let mut states = HashMap::new();
    let mut current_state: char = ' ';
    let mut current_rule_index: usize = 0;
    let mut rules_for_state: [Rule; 2] = [
        Rule {
            write_value: 0,
            move_direction: 0,
            next_state: ' ',
        },
        Rule {
            write_value: 0,
            move_direction: 0,
            next_state: ' ',
        },
    ];
    for line in lines {
        let trimmed_line = line.trim();
        if trimmed_line.starts_with("In state") {
            if current_state != ' ' {
                states.insert(
                    current_state,
                    State {
                        rules: rules_for_state,
                    },
                );
            }
            current_state = trimmed_line
                .split_whitespace()
                .last()
                .unwrap()
                .chars()
                .next()
                .unwrap();
            current_rule_index = 0;
            rules_for_state = [
                Rule {
                    write_value: 0,
                    move_direction: 0,
                    next_state: ' ',
                },
                Rule {
                    write_value: 0,
                    move_direction: 0,
                    next_state: ' ',
                },
            ];
        } else if trimmed_line.starts_with("If the current value is") {
            //Skip this
        } else if trimmed_line.starts_with("- Write the value") {
            let value = trimmed_line
                .split_whitespace()
                .last()
                .unwrap()
                .trim_end_matches('.')
                .parse::<u8>()
                .unwrap();
            rules_for_state[current_rule_index].write_value = value;
        } else if trimmed_line.starts_with("- Move one slot to the") {
            let direction = if trimmed_line.contains("right") { 1 } else { -1 };
            rules_for_state[current_rule_index].move_direction = direction;
        } else if trimmed_line.starts_with("- Continue with state") {
            let next_state = trimmed_line
                .split_whitespace()
                .last()
                .unwrap()
                .trim_end_matches('.')
                .chars()
                .next()
                .unwrap();
            rules_for_state[current_rule_index].next_state = next_state;
            current_rule_index += 1;
        }
    }
    states.insert(
        current_state,
        State {
            rules: rules_for_state,
        },
    );
    (start_state, steps, states)
}

fn run_turing_machine(start_state: char, steps: usize, states: &HashMap<char, State>) -> usize {
    let mut tape: HashMap<i32, u8> = HashMap::new();
    let mut cursor_position: i32 = 0;
    let mut current_state = start_state;

    for _ in 0..steps {
        let current_value = tape.get(&cursor_position).unwrap_or(&0).clone();
        let rule_index = current_value as usize;

        let current_state_rules = states.get(&current_state).unwrap();
        let rule = &current_state_rules.rules[rule_index];

        tape.insert(cursor_position, rule.write_value);
        cursor_position += rule.move_direction;
        current_state = rule.next_state;
    }

    tape.values().filter(|&val| *val == 1).count()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let (start_state, steps, states) = parse_input(&input);
    let checksum = run_turing_machine(start_state, steps, &states);
    println!("{}", checksum);
}
