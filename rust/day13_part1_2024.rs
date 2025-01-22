
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct Machine {
    ax: i32,
    ay: i32,
    bx: i32,
    by: i32,
    px: i32,
    py: i32,
}

fn main() {
    let machines = read_input("input.txt");
    let mut results = Vec::new();

    for m in machines {
        if let Some(cost) = solve_machine(&m) {
            results.push(cost);
        }
    }

    if results.is_empty() {
        println!("0 0");
        return;
    }

    let count = results.len();
    let sum: i32 = results.iter().sum();
    println!("{} {}", count, sum);
}

fn read_input(filename: &str) -> Vec<Machine> {
    let file = File::open(filename).expect("Unable to open file");
    let reader = BufReader::new(file);

    let mut machines = Vec::new();
    let mut lines = Vec::new();

    for line in reader.lines() {
        let line = line.expect("Unable to read line").trim().to_string();
        if line.is_empty() {
            if !lines.is_empty() {
                machines.push(parse_machine(&lines));
                lines.clear();
            }
        } else {
            lines.push(line);
        }
    }

    if !lines.is_empty() {
        machines.push(parse_machine(&lines));
    }

    machines
}

fn parse_machine(lines: &[String]) -> Machine {
    let mut m = Machine {
        ax: 0,
        ay: 0,
        bx: 0,
        by: 0,
        px: 0,
        py: 0,
    };

    for line in lines {
        let line = line
            .replace("Button A:", "A:")
            .replace("Button B:", "B:")
            .replace("Prize:", "P:");

        if line.starts_with("A:") {
            let (x, y) = parse_line(&line[2..]);
            m.ax = x;
            m.ay = y;
        } else if line.starts_with("B:") {
            let (x, y) = parse_line(&line[2..]);
            m.bx = x;
            m.by = y;
        } else if line.starts_with("P:") {
            let (x, y) = parse_prize(&line[2..]);
            m.px = x;
            m.py = y;
        }
    }
    m
}

fn parse_line(s: &str) -> (i32, i32) {
    let parts: Vec<&str> = s.trim().split(',').collect();
    let x = parse_val(parts[0].trim());
    let y = parse_val(parts[1].trim());
    (x, y)
}

fn parse_prize(s: &str) -> (i32, i32) {
    let parts: Vec<&str> = s.trim().split(',').collect();
    let x = parse_val_prize(parts[0].trim());
    let y = parse_val_prize(parts[1].trim());
    (x, y)
}

fn parse_val(s: &str) -> i32 {
    let s = s
        .trim()
        .trim_start_matches("X+")
        .trim_start_matches("Y+")
        .trim_start_matches("X=")
        .trim_start_matches("Y=");
    s.parse().unwrap_or(0)
}

fn parse_val_prize(s: &str) -> i32 {
    let s = s.trim().trim_start_matches("X=").trim_start_matches("Y=");
    s.parse().unwrap_or(0)
}

fn solve_machine(m: &Machine) -> Option<i32> {
    let mut min_cost = None;
    let max_presses = 100;
    
    for a_count in 0..=max_presses {
        for b_count in 0..=max_presses {
            let x = m.ax * a_count + m.bx * b_count;
            let y = m.ay * a_count + m.by * b_count;
           
            if x == m.px && y == m.py {
                let cost = a_count * 3 + b_count;
                min_cost = match min_cost {
                     None => Some(cost),
                     Some(min) => Some(std::cmp::min(min,cost)),
                }
            }
        }
    }
    min_cost
}
