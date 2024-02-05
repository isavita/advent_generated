
use std::collections::HashMap;
use std::fs;

fn calculate_number(monkey: &str, monkeys: &HashMap<&str, &str>, numbers: &HashMap<&str, i64>) -> i64 {
    if let Some(num) = numbers.get(monkey) {
        return *num;
    }

    let job = monkeys.get(monkey).unwrap();
    let parts: Vec<&str> = job.split_whitespace().collect();

    let result = match parts[1] {
        "+" => calculate_number(parts[0], monkeys, numbers) + calculate_number(parts[2], monkeys, numbers),
        "-" => calculate_number(parts[0], monkeys, numbers) - calculate_number(parts[2], monkeys, numbers),
        "*" => calculate_number(parts[0], monkeys, numbers) * calculate_number(parts[2], monkeys, numbers),
        "/" => calculate_number(parts[0], monkeys, numbers) / calculate_number(parts[2], monkeys, numbers),
        _ => 0,
    };

    result
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut monkeys: HashMap<&str, &str> = HashMap::new();
    let mut numbers: HashMap<&str, i64> = HashMap::new();

    for line in input.lines() {
        let parts: Vec<&str> = line.split(": ").collect();
        if parts[1].contains(|c: char| c.is_numeric()) {
            numbers.insert(parts[0], parts[1].parse().unwrap());
        } else {
            monkeys.insert(parts[0], parts[1]);
        }
    }

    let result = calculate_number("root", &monkeys, &numbers);
    println!("{}", result);
}
