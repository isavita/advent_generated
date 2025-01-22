
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug, Clone)]
enum Rule {
    Char(char),
    Sequence(Vec<usize>),
    Or(Vec<Vec<usize>>),
}

fn parse_rules(lines: &mut std::io::Lines<std::io::BufReader<File>>) -> HashMap<usize, Rule> {
    let mut rules = HashMap::new();
    for line in lines.by_ref() {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }
        let parts: Vec<&str> = line.split(": ").collect();
        let rule_id: usize = parts[0].parse().unwrap();
        let rule_str = parts[1];

        let rule = if rule_str.starts_with('"') {
            Rule::Char(rule_str.chars().nth(1).unwrap())
        } else if rule_str.contains('|') {
            let or_parts: Vec<&str> = rule_str.split(" | ").collect();
            let or_sequences: Vec<Vec<usize>> = or_parts
                .iter()
                .map(|part| {
                    part.split_whitespace()
                        .map(|num| num.parse().unwrap())
                        .collect()
                })
                .collect();
            Rule::Or(or_sequences)
        } else {
            let seq: Vec<usize> = rule_str
                .split_whitespace()
                .map(|num| num.parse().unwrap())
                .collect();
            Rule::Sequence(seq)
        };

        rules.insert(rule_id, rule);
    }
    rules
}

fn matches(
    rules: &HashMap<usize, Rule>,
    rule_id: usize,
    message: &str,
    pos: usize,
) -> Vec<usize> {
    if pos >= message.len() {
        return vec![];
    }

    match &rules[&rule_id] {
        Rule::Char(c) => {
            if message.chars().nth(pos) == Some(*c) {
                vec![pos + 1]
            } else {
                vec![]
            }
        }
        Rule::Sequence(seq) => {
            let mut positions = vec![pos];
            for &sub_rule_id in seq {
                let mut next_positions = Vec::new();
                for p in positions {
                    next_positions.extend(matches(rules, sub_rule_id, message, p));
                }
                positions = next_positions;
                if positions.is_empty() {
                  return vec![];
                }
            }
            positions
        }
        Rule::Or(or_sequences) => {
          let mut result = Vec::new();
          for seq in or_sequences {
            let mut positions = vec![pos];
            for &sub_rule_id in seq {
                let mut next_positions = Vec::new();
                for p in positions {
                    next_positions.extend(matches(rules, sub_rule_id, message, p));
                }
                positions = next_positions;
                if positions.is_empty() {
                  break;
                }
              }
              result.extend(positions);
          }
          result
        }
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let mut lines = reader.lines();

    let rules = parse_rules(&mut lines);
    let mut valid_messages_count = 0;

    for line in lines {
        let message = line.unwrap();
        let matching_positions = matches(&rules, 0, &message, 0);
        if matching_positions.contains(&message.len()) {
            valid_messages_count += 1;
        }
    }

    println!("{}", valid_messages_count);

    Ok(())
}
