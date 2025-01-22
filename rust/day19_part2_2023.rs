
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug)]
enum Rule {
    Conditional {
        category: char,
        operator: char,
        value: i32,
        destination: String,
    },
    Unconditional {
        destination: String,
    },
}

#[derive(Debug, Clone, Copy)]
struct Part {
    x: i32,
    m: i32,
    a: i32,
    s: i32,
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn parse_workflows(lines: &mut impl Iterator<Item = String>) -> HashMap<String, Vec<Rule>> {
    let mut workflows = HashMap::new();
    for line in lines {
        if line.is_empty() {
            break;
        }
        let parts: Vec<&str> = line.split('{').collect();
        let name = parts[0].to_string();
        let rules_str = parts[1].trim_end_matches('}');
        let rules_parts: Vec<&str> = rules_str.split(',').collect();
        let mut rules = Vec::new();
        for rule_part in rules_parts {
            if rule_part.contains(':') {
                let rule_pieces: Vec<&str> = rule_part.split(':').collect();
                let condition = rule_pieces[0];
                let destination = rule_pieces[1].to_string();
                let category = condition.chars().next().unwrap();
                let operator = condition.chars().nth(1).unwrap();
                let value = condition[2..].parse().unwrap();
                rules.push(Rule::Conditional {
                    category,
                    operator,
                    value,
                    destination,
                });
            } else {
                rules.push(Rule::Unconditional {
                    destination: rule_part.to_string(),
                });
            }
        }
        workflows.insert(name, rules);
    }
    workflows
}

fn parse_parts(lines: &mut impl Iterator<Item = String>) -> Vec<Part> {
    let mut parts = Vec::new();
    for line in lines {
        let line = line.trim_matches(|c| c == '{' || c == '}');
        let parts_values: Vec<&str> = line.split(',').collect();
        let mut part = Part { x: 0, m: 0, a: 0, s: 0 };
        for part_value in parts_values {
            let value: i32 = part_value[2..].parse().unwrap();
            match part_value.chars().next().unwrap() {
                'x' => part.x = value,
                'm' => part.m = value,
                'a' => part.a = value,
                's' => part.s = value,
                _ => panic!("Invalid category"),
            }
        }
        parts.push(part);
    }
    parts
}

fn process_part(
    part: Part,
    workflows: &HashMap<String, Vec<Rule>>,
    start_workflow: &str,
) -> bool {
    let mut current_workflow = start_workflow.to_string();
    loop {
        match current_workflow.as_str() {
            "A" => return true,
            "R" => return false,
            _ => {
                let rules = workflows.get(&current_workflow).unwrap();
                let mut next_workflow = None;
                for rule in rules {
                    match rule {
                        Rule::Conditional {
                            category,
                            operator,
                            value,
                            destination,
                        } => {
                            let part_value = match category {
                                'x' => part.x,
                                'm' => part.m,
                                'a' => part.a,
                                's' => part.s,
                                _ => panic!("Invalid category"),
                            };
                            let condition_met = match operator {
                                '<' => part_value < *value,
                                '>' => part_value > *value,
                                _ => panic!("Invalid operator"),
                            };
                            if condition_met {
                                next_workflow = Some(destination.clone());
                                break;
                            }
                        }
                        Rule::Unconditional { destination } => {
                            next_workflow = Some(destination.clone());
                            break;
                        }
                    }
                }
                current_workflow = next_workflow.unwrap();
            }
        }
    }
}


fn solve_part1(workflows: &HashMap<String, Vec<Rule>>, parts: &Vec<Part>) -> i32 {
    let mut total = 0;
    for part in parts {
        if process_part(*part, workflows, "in") {
            total += part.x + part.m + part.a + part.s;
        }
    }
    total
}

#[derive(Debug, Clone, Copy)]
struct PartRange {
    x_min: i32,
    x_max: i32,
    m_min: i32,
    m_max: i32,
    a_min: i32,
    a_max: i32,
    s_min: i32,
    s_max: i32,
}

fn count_accepted_combinations(workflows: &HashMap<String, Vec<Rule>>) -> i64 {
    let mut total = 0;
    let initial_range = PartRange {
        x_min: 1, x_max: 4000,
        m_min: 1, m_max: 4000,
        a_min: 1, a_max: 4000,
        s_min: 1, s_max: 4000,
    };
    
    let mut queue = vec![("in".to_string(), initial_range)];

    while let Some((workflow_name, current_range)) = queue.pop(){

        if workflow_name == "A" {
            let combinations = (current_range.x_max as i64 - current_range.x_min as i64 + 1) *
                (current_range.m_max as i64 - current_range.m_min as i64 + 1) *
                (current_range.a_max as i64 - current_range.a_min as i64 + 1) *
                (current_range.s_max as i64 - current_range.s_min as i64 + 1);
            total += combinations;
            continue;
        }

        if workflow_name == "R" {
            continue;
        }

        let rules = workflows.get(&workflow_name).unwrap();

        let mut current_range = current_range;

        for rule in rules {
            match rule {
                Rule::Conditional { category, operator, value, destination } => {
                   let mut next_range = current_range;

                    match category{
                        'x' => match operator{
                            '<' => {
                                next_range.x_max = i32::min(current_range.x_max, value - 1);
                                current_range.x_min = i32::max(current_range.x_min, *value);
                            },
                            '>' => {
                                next_range.x_min = i32::max(current_range.x_min, value + 1);
                                current_range.x_max = i32::min(current_range.x_max, *value);
                            },
                            _ => panic!("Invalid operator"),
                        },
                        'm' => match operator {
                            '<' => {
                                next_range.m_max = i32::min(current_range.m_max, value - 1);
                                current_range.m_min = i32::max(current_range.m_min, *value);
                            },
                             '>' => {
                                next_range.m_min = i32::max(current_range.m_min, value + 1);
                                current_range.m_max = i32::min(current_range.m_max, *value);
                            },
                            _ => panic!("Invalid operator"),
                        },
                         'a' => match operator {
                           '<' => {
                                next_range.a_max = i32::min(current_range.a_max, value - 1);
                                current_range.a_min = i32::max(current_range.a_min, *value);
                            },
                             '>' => {
                                next_range.a_min = i32::max(current_range.a_min, value + 1);
                                current_range.a_max = i32::min(current_range.a_max, *value);
                            },
                            _ => panic!("Invalid operator"),
                        },
                         's' => match operator {
                             '<' => {
                                next_range.s_max = i32::min(current_range.s_max, value - 1);
                                current_range.s_min = i32::max(current_range.s_min, *value);
                            },
                             '>' => {
                                next_range.s_min = i32::max(current_range.s_min, value + 1);
                                current_range.s_max = i32::min(current_range.s_max, *value);
                            },
                            _ => panic!("Invalid operator"),
                        },
                        _ => panic!("Invalid category"),
                    }
                    if next_range.x_min <= next_range.x_max &&
                        next_range.m_min <= next_range.m_max &&
                        next_range.a_min <= next_range.a_max &&
                        next_range.s_min <= next_range.s_max {
                        queue.push((destination.clone(), next_range));
                    }
                },
                Rule::Unconditional { destination } => {
                    queue.push((destination.clone(), current_range));
                },
            }
        }
    }

    total
}

fn main() -> io::Result<()> {
    let lines = read_lines("input.txt")?;
    let mut lines_iter = lines.map(Result::unwrap);

    let workflows = parse_workflows(&mut lines_iter);
    let parts = parse_parts(&mut lines_iter);

    let part1_answer = solve_part1(&workflows, &parts);
    println!("Part 1 Answer: {}", part1_answer);
    
    let part2_answer = count_accepted_combinations(&workflows);
    println!("Part 2 Answer: {}", part2_answer);

    Ok(())
}
