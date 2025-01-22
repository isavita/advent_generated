
use std::fs;

#[derive(Debug)]
struct Monkey {
    items: Vec<i64>,
    operation: Operation,
    div: i64,
    next: [usize; 2],
    inspections: i64,
}

#[derive(Debug)]
enum Operation {
    Add(i64),
    Mul(i64),
    Square,
}

fn parse_monkey(s: &str) -> Monkey {
    let lines: Vec<&str> = s.lines().collect();
    let items: Vec<i64> = lines[1]
        .split(": ")
        .nth(1)
        .unwrap()
        .split(", ")
        .map(|x| x.parse().unwrap())
        .collect();
    let parts: Vec<&str> = lines[2].split("= ").nth(1).unwrap().split_whitespace().collect();
    let operation = match parts[1] {
        "+" => match parts[2] {
            "old" => Operation::Add(0),
            x => Operation::Add(x.parse().unwrap()),
        },
        "*" => match parts[2] {
            "old" => Operation::Square,
            x => Operation::Mul(x.parse().unwrap()),
        },
        _ => panic!("invalid operation"),
    };
    let div = lines[3]
        .split("divisible by ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();
    let next0 = lines[4]
        .split("monkey ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();
    let next1 = lines[5]
        .split("monkey ")
        .nth(1)
        .unwrap()
        .parse()
        .unwrap();
    Monkey {
        items,
        operation,
        div,
        next: [next0, next1],
        inspections: 0,
    }
}

fn monkey_business(monkeys: &mut Vec<Monkey>, rounds: i32, worry: bool) -> i64 {
    let div: i64 = monkeys.iter().map(|m| m.div).product();
    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            let mut new_items: Vec<(usize, i64)> = Vec::new();
            let m = &mut monkeys[i];
            m.inspections += m.items.len() as i64;
            for item in &m.items {
                let mut new_val = match m.operation {
                    Operation::Add(x) => item + x,
                    Operation::Mul(x) => item * x,
                    Operation::Square => item * item,
                };
                if worry {
                   new_val %= div;
                } else {
                   new_val /= 3;
                }

               if new_val % m.div == 0 {
                    new_items.push((m.next[0],new_val));
                } else {
                    new_items.push((m.next[1],new_val));
                }
            }
            m.items.clear();
            for (next_monkey,item) in new_items {
                monkeys[next_monkey].items.push(item);
            }
        }
    }
    let mut inspections: Vec<i64> = monkeys.iter().map(|m| m.inspections).collect();
    inspections.sort_by(|a, b| b.cmp(a));
    inspections[0] * inspections[1]
}

fn main() {
    let s = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut monkeys: Vec<Monkey> = s.split("\n\n").map(parse_monkey).collect();
    println!("{}", monkey_business(&mut monkeys, 20, false));
    let mut monkeys: Vec<Monkey> = s.split("\n\n").map(parse_monkey).collect();
    println!("{}", monkey_business(&mut monkeys, 10000, true));
}
