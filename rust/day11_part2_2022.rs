
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
    Multiply(i64),
    Square,
}

fn parse(s: &str) -> Monkey {
    let lines: Vec<&str> = s.lines().collect();
    let items: Vec<i64> = lines[1]
        .split(": ")
        .nth(1)
        .unwrap()
        .split(", ")
        .map(|item| item.parse().unwrap())
        .collect();

    let f: Vec<&str> = lines[2].split("= ").nth(1).unwrap().split_whitespace().collect();
    let operation = match f[1] {
        "+" => match f[2] {
            "old" => Operation::Square,
            _ => Operation::Add(f[2].parse().unwrap()),
        },
        "*" => match f[2] {
            "old" => Operation::Square,
            _ => Operation::Multiply(f[2].parse().unwrap()),
        },
        _ => panic!("invalid operation"),
    };

    let div: i64 = lines[3].split("by ").nth(1).unwrap().parse().unwrap();
    let next_true: usize = lines[4].split("monkey ").nth(1).unwrap().parse().unwrap();
    let next_false: usize = lines[5].split("monkey ").nth(1).unwrap().parse().unwrap();

    Monkey {
        items,
        operation,
        div,
        next: [next_true, next_false],
        inspections: 0,
    }
}

fn monkey_business(monkeys: &mut Vec<Monkey>, rounds: usize, worry: bool) -> i64 {
    let div: i64 = monkeys.iter().map(|m| m.div).product();

    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            let mut items = std::mem::take(&mut monkeys[i].items);
            for item in items {
                monkeys[i].inspections += 1;
                let mut new_item = match &monkeys[i].operation {
                    Operation::Add(val) => item + val,
                    Operation::Multiply(val) => item * val,
                    Operation::Square => item * item,
                };
                 if worry {
                    new_item %= div;
                } else {
                    new_item /= 3;
                }

                let next_monkey = if new_item % monkeys[i].div == 0 {
                    monkeys[i].next[0]
                } else {
                    monkeys[i].next[1]
                };
                 monkeys[next_monkey].items.push(new_item);
            }
        }
    }
    monkeys.sort_by(|a, b| b.inspections.cmp(&a.inspections));
    monkeys[0].inspections * monkeys[1].inspections
}

fn main() {
    let s = fs::read_to_string("input.txt").unwrap();
    let mut monkeys: Vec<Monkey> = s.split("\n\n").map(parse).collect();
    println!("{}", monkey_business(&mut monkeys, 10000, true));
}
