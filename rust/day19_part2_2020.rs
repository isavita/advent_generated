
use std::collections::HashMap;
use std::fs;
use std::str::FromStr;

#[derive(Debug, Clone)]
enum Rule {
    Char(char),
    Sequence(Vec<usize>),
    Or(Vec<Vec<usize>>),
}

fn parse_rule(line: &str) -> (usize, Rule) {
    let parts: Vec<&str> = line.split(": ").collect();
    let id = usize::from_str(parts[0]).unwrap();
    let rule_str = parts[1];

    if rule_str.starts_with('"') {
        let c = rule_str.chars().nth(1).unwrap();
        (id, Rule::Char(c))
    } else if rule_str.contains("|") {
        let or_parts: Vec<&str> = rule_str.split(" | ").collect();
        let or_sequences: Vec<Vec<usize>> = or_parts
            .iter()
            .map(|s| {
                s.split_whitespace()
                    .map(|num| usize::from_str(num).unwrap())
                    .collect()
            })
            .collect();
        (id, Rule::Or(or_sequences))
    } else {
        let sequence: Vec<usize> = rule_str
            .split_whitespace()
            .map(|num| usize::from_str(num).unwrap())
            .collect();
        (id, Rule::Sequence(sequence))
    }
}

fn matches<'a>(
    rules: &HashMap<usize, Rule>,
    rule_id: usize,
    message: &'a str,
    memo: &mut HashMap<(usize, &'a str), Vec<&'a str>>,
) -> Vec<&'a str> {
    if let Some(res) = memo.get(&(rule_id, message)) {
        return res.clone();
    }

    let rule = &rules[&rule_id];
    let result: Vec<&'a str> = match rule {
        Rule::Char(c) => {
            if message.starts_with(*c) {
                vec![&message[1..]]
            } else {
                vec![]
            }
        }
        Rule::Sequence(seq) => {
            let mut results = vec![message];
            for &sub_rule in seq {
                let mut next_results = Vec::new();
                for res in results {
                    next_results.extend(matches(rules, sub_rule, res, memo));
                }
                results = next_results;
            }
            results
        }
        Rule::Or(or_rules) => {
            let mut or_results = Vec::new();
            for seq in or_rules {
                let mut results = vec![message];
                for &sub_rule in seq {
                    let mut next_results = Vec::new();
                    for res in results {
                        next_results.extend(matches(rules, sub_rule, res, memo));
                    }
                    results = next_results;
                }
                or_results.extend(results);
            }
            or_results
        }
    };

    memo.insert((rule_id, message), result.clone());
    result
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let parts: Vec<&str> = input.split("\n\n").collect();
    let rule_lines: Vec<&str> = parts[0].lines().collect();
    let messages: Vec<&str> = parts[1].lines().collect();

    let mut rules: HashMap<usize, Rule> = HashMap::new();
    for line in rule_lines {
        let (id, rule) = parse_rule(line);
        rules.insert(id, rule);
    }

    // Part 2 modifications
    rules.insert(8, Rule::Or(vec![vec![42], vec![42, 8]]));
    rules.insert(11, Rule::Or(vec![vec![42, 31], vec![42, 11, 31]]));

    let mut count = 0;
    for message in messages {
        let mut memo: HashMap<(usize, &str), Vec<&str>> = HashMap::new();
        let remaining = matches(&rules, 0, message, &mut memo);
        if remaining.iter().any(|&rem| rem.is_empty()) {
            count += 1;
        }
    }
    println!("{}", count);
}
