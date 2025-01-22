
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

#[derive(Debug)]
struct Machine {
    ax: i64,
    ay: i64,
    bx: i64,
    by: i64,
    px: i64,
    py: i64,
}

fn main() -> io::Result<()> {
    const OFFSET: i64 = 1_000_000_000_000_0;
    let machines = read_input("input.txt")?;
    let mut results = Vec::new();
    for mut m in machines {
        m.px += OFFSET;
        m.py += OFFSET;
        if let Some(cost) = solve_machine(&m) {
            results.push(cost);
        }
    }
    if results.is_empty() {
        println!("0 0");
    } else {
        let count = results.len();
        let sum: i64 = results.iter().sum();
        println!("{} {}", count, sum);
    }
    Ok(())
}

fn read_input(filename: &str) -> io::Result<Vec<Machine>> {
    let file = File::open(filename)?;
    let reader = io::BufReader::new(file);
    let mut machines = Vec::new();
    let mut lines = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let line = line.trim();
        if line.is_empty() {
            if !lines.is_empty() {
                machines.push(parse_machine(&lines));
                lines.clear();
            }
        } else {
            lines.push(line.to_string());
        }
    }
        if !lines.is_empty() {
        machines.push(parse_machine(&lines));
    }
    Ok(machines)
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
    for l in lines {
        let l = l.replace("Button A:", "A:");
        let l = l.replace("Button B:", "B:");
        let l = l.replace("Prize:", "P:");
        if l.starts_with("A:") {
            let (x, y) = parse_line(&l[2..]);
            m.ax = x;
            m.ay = y;
        } else if l.starts_with("B:") {
            let (x, y) = parse_line(&l[2..]);
            m.bx = x;
            m.by = y;
        } else if l.starts_with("P:") {
           let (x, y) = parse_prize(&l[2..]);
            m.px = x;
            m.py = y;
        }
    }
    m
}

fn parse_line(s: &str) -> (i64, i64) {
    let parts: Vec<&str> = s.trim().split(',').collect();
    let x = parse_val(parts[0]);
    let y = parse_val(parts[1]);
    (x, y)
}

fn parse_prize(s: &str) -> (i64, i64) {
    let parts: Vec<&str> = s.trim().split(',').collect();
      let x = parse_val_prize(parts[0]);
    let y = parse_val_prize(parts[1]);
    (x, y)
}


fn parse_val(s: &str) -> i64 {
    let s = s.trim();
    let s = s.trim_start_matches("X+");
    let s = s.trim_start_matches("Y+");
    let s = s.trim_start_matches("X=");
     let s = s.trim_start_matches("Y=");
    i64::from_str(s).unwrap()
}

fn parse_val_prize(s: &str) -> i64 {
    let s = s.trim();
    let s = s.trim_start_matches("X=");
     let s = s.trim_start_matches("Y=");
    i64::from_str(s).unwrap()
}


fn solve_machine(m: &Machine) -> Option<i64> {
    let d = m.ax * m.by - m.ay * m.bx;
    if d == 0 {
        return None;
    }
    let num_a = m.px * m.by - m.py * m.bx;
    let num_b = -m.px * m.ay + m.py * m.ax;

    if num_a % d != 0 || num_b % d != 0 {
        return None;
    }
    let a = num_a / d;
    let b = num_b / d;
    if a < 0 || b < 0 {
        return None;
    }
    Some(3 * a + b)
}
