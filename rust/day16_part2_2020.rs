
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::{HashMap, HashSet};

struct Rule {
    name: String,
    ranges: Vec<(i32, i32)>,
}

impl Rule {
    fn is_valid(&self, value: i32) -> bool {
        self.ranges.iter().any(|&(min, max)| value >= min && value <= max)
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut rules = Vec::new();
    let mut my_ticket = Vec::new();
    let mut nearby_tickets = Vec::new();
    let mut section = 0;

    for line_result in reader.lines() {
        let line = line_result?;
        if line.is_empty() {
            section += 1;
            continue;
        }

        match section {
            0 => {
                let parts: Vec<&str> = line.split(": ").collect();
                let name = parts[0].to_string();
                let range_parts: Vec<&str> = parts[1].split(" or ").collect();
                let mut ranges = Vec::new();
                for part in range_parts {
                    let nums: Vec<i32> = part.split("-").map(|s| s.parse().unwrap()).collect();
                    ranges.push((nums[0], nums[1]));
                }
                rules.push(Rule { name, ranges });
            }
            1 => {
                if line != "your ticket:" {
                    my_ticket = parse_ticket(&line);
                }
            }
            2 => {
                if line != "nearby tickets:" {
                    let ticket = parse_ticket(&line);
                      if is_valid_ticket(&ticket, &rules) {
                        nearby_tickets.push(ticket);
                     }
                }
            }
            _ => {}
        }
    }

    let field_positions = solve_field_positions(&rules, &nearby_tickets);
    let departure_product = calculate_departure_product(&my_ticket, &field_positions);

    println!("{}", departure_product);
    Ok(())
}

fn parse_ticket(s: &str) -> Vec<i32> {
    s.split(",").map(|v| v.parse().unwrap()).collect()
}

fn is_valid_ticket(ticket: &[i32], rules: &[Rule]) -> bool {
    ticket.iter().all(|&value| is_valid_for_any_rule(value, rules))
}

fn is_valid_for_any_rule(value: i32, rules: &[Rule]) -> bool {
    rules.iter().any(|rule| rule.is_valid(value))
}

fn solve_field_positions(rules: &[Rule], tickets: &[Vec<i32>]) -> HashMap<String, usize> {
    let mut valid_positions: HashMap<String, HashSet<usize>> = HashMap::new();
    for rule in rules {
        let mut positions = HashSet::new();
        for i in 0..tickets[0].len() {
             if tickets.iter().all(|ticket| rule.is_valid(ticket[i]))
               {
                   positions.insert(i);
               }
         }
         valid_positions.insert(rule.name.clone(), positions);
    }

    let mut field_positions: HashMap<String, usize> = HashMap::new();
     while field_positions.len() < rules.len() {
         for (name, positions) in valid_positions.clone() {
             if positions.len() == 1 {
                let pos = *positions.iter().next().unwrap();
                field_positions.insert(name.clone(), pos);
                 for (_, other_positions) in valid_positions.iter_mut() {
                     other_positions.remove(&pos);
                 }
             }
         }
         valid_positions.retain(|_, positions| !positions.is_empty());
    }

    field_positions
}


fn calculate_departure_product(ticket: &[i32], field_positions: &HashMap<String, usize>) -> i64 {
    field_positions
        .iter()
        .filter(|(name, _)| name.starts_with("departure"))
        .map(|(_, &pos)| ticket[pos] as i64)
        .product()
}
