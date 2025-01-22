
use std::collections::HashMap;
use std::fs;
use std::str::FromStr;

#[derive(Debug, Clone)]
struct Rule {
    category: Option<u8>,
    operator: Option<u8>,
    num: i32,
    workflow_name: String,
}

type Workflows = HashMap<String, Vec<Rule>>;
type Part = HashMap<u8, i32>;

#[derive(Debug, Clone)]
struct Interval {
    start: i32,
    end: i32,
}

type PartInterval = HashMap<u8, Interval>;

fn parse_input(input: &str) -> (Workflows, Vec<Part>) {
    let mut workflows = Workflows::new();
    let mut parts = Vec::new();
    let mut lines = input.lines();

    for line in &mut lines {
        if line.is_empty() {
            break;
        }
        let (workflow_name, rules) = parse_workflow(line);
        workflows.insert(workflow_name, rules);
    }

    for line in lines {
        let part = parse_part(line);
        parts.push(part);
    }

    (workflows, parts)
}

fn parse_workflow(line: &str) -> (String, Vec<Rule>) {
    let idx = line.find('{').unwrap();
    let workflow_name = line[0..idx].to_string();
    let rules_str = &line[idx + 1..line.len() - 1];
    let rules_strs = rules_str.split(',');
    let mut rules = Vec::new();
    for rule_str in rules_strs {
        let rule = if let Some(idx) = rule_str.find(':') {
            let category = rule_str.as_bytes()[0];
            let operator = rule_str.as_bytes()[1];
            let num = i32::from_str(&rule_str[2..idx]).unwrap();
            let workflow_name = rule_str[idx + 1..].to_string();
            Rule {
                category: Some(category),
                operator: Some(operator),
                num,
                workflow_name,
            }
        } else {
            Rule {
                category: None,
                operator: None,
                num: 0,
                workflow_name: rule_str.to_string(),
            }
        };
        rules.push(rule);
    }
    (workflow_name, rules)
}

fn parse_part(line: &str) -> Part {
    let mut part = Part::new();
    let mut parts = line[1..line.len() - 1].split(',');
    for p in parts {
        let mut kv = p.split('=');
        let k = kv.next().unwrap().as_bytes()[0];
        let v = i32::from_str(kv.next().unwrap()).unwrap();
        part.insert(k, v);
    }
    part
}

fn apply_workflow(part: &Part, workflows: &Workflows, workflow_name: &str) -> bool {
    if workflow_name == "A" {
        return true;
    }
    if workflow_name == "R" {
        return false;
    }

    if let Some(rules) = workflows.get(workflow_name) {
        for rule in rules {
            if let Some(category) = rule.category {
                let rating = part[&category];
                let is_valid = match rule.operator.unwrap() {
                    b'>' => rating > rule.num,
                    b'<' => rating < rule.num,
                    _ => true,
                };
                if is_valid {
                    return apply_workflow(part, workflows, &rule.workflow_name);
                }
            } else {
                return apply_workflow(part, workflows, &rule.workflow_name);
            }
        }
    }
    false
}

fn solve(input: &str) -> i32 {
    let start_workflow = "in";
    let (workflows, parts) = parse_input(input);
    let mut res = 0;
    for part in parts {
        if apply_workflow(&part, &workflows, start_workflow) {
            res += part.values().sum::<i32>();
        }
    }
    res
}

fn read_file(filename: &str) -> String {
    fs::read_to_string(filename).expect("Unable to read file")
}

fn main() {
    let input = read_file("input.txt");
    println!("{}", solve(&input));
}
