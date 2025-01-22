
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

#[derive(Debug, Clone)]
enum SnailfishNumber {
    Regular(u32),
    Pair(Box<SnailfishNumber>, Box<SnailfishNumber>),
}

impl FromStr for SnailfishNumber {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_snailfish_number(s)
    }
}

fn parse_snailfish_number(s: &str) -> Result<SnailfishNumber, String> {
    let mut chars = s.chars().peekable();
    parse_snailfish_number_inner(&mut chars)
}

fn parse_snailfish_number_inner(
    chars: &mut std::iter::Peekable<std::str::Chars>,
) -> Result<SnailfishNumber, String> {
    match chars.peek() {
        Some('[') => {
            chars.next();
            let left = parse_snailfish_number_inner(chars)?;
            match chars.next() {
                Some(',') => {}
                _ => return Err("Expected comma after left element".to_string()),
            }
            let right = parse_snailfish_number_inner(chars)?;
            match chars.next() {
                Some(']') => {}
                _ => return Err("Expected closing bracket after right element".to_string()),
            }
            Ok(SnailfishNumber::Pair(Box::new(left), Box::new(right)))
        }
        Some(c) if c.is_digit(10) => {
            let mut num_str = String::new();
            while let Some(&c) = chars.peek() {
                if c.is_digit(10) {
                    num_str.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            let num = num_str.parse::<u32>().map_err(|e| e.to_string())?;
            Ok(SnailfishNumber::Regular(num))
        }
        _ => Err("Invalid input".to_string()),
    }
}

fn add(a: SnailfishNumber, b: SnailfishNumber) -> SnailfishNumber {
    let mut result = SnailfishNumber::Pair(Box::new(a), Box::new(b));
    reduce(&mut result);
    result
}

fn reduce(number: &mut SnailfishNumber) {
    loop {
        if explode(number, 0).0 {
            continue;
        }
        if split(number) {
            continue;
        }
        break;
    }
}

fn explode(number: &mut SnailfishNumber, depth: u32) -> (bool, Option<u32>, Option<u32>) {
    match number {
        SnailfishNumber::Regular(_) => (false, None, None),
        SnailfishNumber::Pair(left, right) => {
            if depth >= 4 {
                if let (SnailfishNumber::Regular(l), SnailfishNumber::Regular(r)) =
                    (&**left, &**right)
                {
                    let left_val = Some(*l);
                    let right_val = Some(*r);
                    *number = SnailfishNumber::Regular(0);
                    return (true, left_val, right_val);
                }
            }
            let (exploded, left_carry, right_carry) = explode(left, depth + 1);
            if exploded {
                if let Some(right_carry_val) = right_carry {
                    add_to_first_regular_left(right, right_carry_val);
                }
                 return (true, left_carry, None);
            }

            let (exploded, left_carry, right_carry) = explode(right, depth + 1);
            if exploded {
                 if let Some(left_carry_val) = left_carry {
                    add_to_first_regular_right(left, left_carry_val);
                }
                 return (true, None, right_carry);
            }

            (false, None, None)
        }
    }
}
fn add_to_first_regular_left(number: &mut SnailfishNumber, value: u32) {
    match number {
        SnailfishNumber::Regular(num) => {
            *num += value;
        }
        SnailfishNumber::Pair(left, _right) => {
            add_to_first_regular_left(left, value)
        }
    }
}
fn add_to_first_regular_right(number: &mut SnailfishNumber, value: u32) {
     match number {
        SnailfishNumber::Regular(num) => {
            *num += value;
        }
        SnailfishNumber::Pair(_left, right) => {
             add_to_first_regular_right(right, value)
        }
    }
}
fn split(number: &mut SnailfishNumber) -> bool {
    match number {
        SnailfishNumber::Regular(num) => {
            if *num >= 10 {
                let left = *num / 2;
                let right = (*num + 1) / 2;
                *number = SnailfishNumber::Pair(
                    Box::new(SnailfishNumber::Regular(left)),
                    Box::new(SnailfishNumber::Regular(right)),
                );
                true
            } else {
                false
            }
        }
        SnailfishNumber::Pair(left, right) => {
            if split(left) {
                return true;
            }
            split(right)
        }
    }
}

fn magnitude(number: &SnailfishNumber) -> u32 {
    match number {
        SnailfishNumber::Regular(num) => *num,
        SnailfishNumber::Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right),
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut numbers = reader
        .lines()
        .map(|line| {
            line.map_err(|e| e.to_string())
                .and_then(|s| SnailfishNumber::from_str(&s))
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;

    let final_sum = numbers
        .drain(..)
        .reduce(|a, b| add(a, b))
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "No numbers to add"))?;

    println!("{}", magnitude(&final_sum));
    Ok(())
}
