
use std::{
    fs::File,
    io::{self, BufRead},
    str::FromStr,
};

#[derive(Debug, Clone)]
enum SnailNumber {
    Regular(i32),
    Pair(Box<SnailNumber>, Box<SnailNumber>),
}

impl SnailNumber {
    fn is_regular(&self) -> bool {
        matches!(self, SnailNumber::Regular(_))
    }

    fn add(self, other: SnailNumber) -> Self {
        let mut new_number = SnailNumber::Pair(Box::new(self), Box::new(other));
        new_number.reduce();
        new_number
    }

    fn reduce(&mut self) {
        loop {
            if self.explode(0).0 {
                continue;
            }
            if !self.split() {
                break;
            }
        }
    }

    fn explode(&mut self, depth: usize) -> (bool, i32, i32) {
        if self.is_regular() {
            return (false, 0, 0);
        }

        if depth == 4 {
            if let SnailNumber::Pair(left, right) = self {
                let left_val = match **left {
                    SnailNumber::Regular(v) => v,
                    _ => 0, 
                };
                let right_val = match **right {
                    SnailNumber::Regular(v) => v,
                    _ => 0,
                };
                *self = SnailNumber::Regular(0);
                return (true, left_val, right_val);
            }
        }

        if let SnailNumber::Pair(left, right) = self {
            let (exploded, left_val, right_val) = left.explode(depth + 1);
            if exploded {
                if right_val > 0 {
                   right.add_left(right_val);
                }
                return (true, left_val, 0);
            }

            let (exploded, left_val, right_val) = right.explode(depth + 1);
            if exploded {
                if left_val > 0 {
                   left.add_right(left_val);
                }
                return (true, 0, right_val);
            }
        }

        (false, 0, 0)
    }

    fn add_left(&mut self, value: i32) {
        match self {
            SnailNumber::Regular(v) => *v += value,
            SnailNumber::Pair(left, _) => left.add_left(value),
        }
    }

    fn add_right(&mut self, value: i32) {
        match self {
            SnailNumber::Regular(v) => *v += value,
            SnailNumber::Pair(_, right) => right.add_right(value),
        }
    }


    fn split(&mut self) -> bool {
        match self {
            SnailNumber::Regular(v) => {
                if *v >= 10 {
                    *self = SnailNumber::Pair(
                        Box::new(SnailNumber::Regular(*v / 2)),
                        Box::new(SnailNumber::Regular((*v + 1) / 2)),
                    );
                    true
                } else {
                    false
                }
            }
            SnailNumber::Pair(left, right) => left.split() || right.split(),
        }
    }

    fn magnitude(&self) -> i32 {
        match self {
            SnailNumber::Regular(v) => *v,
            SnailNumber::Pair(left, right) => 3 * left.magnitude() + 2 * right.magnitude(),
        }
    }
}

impl FromStr for SnailNumber {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if !s.starts_with('[') {
            return s.parse::<i32>().map(SnailNumber::Regular).map_err(|e| e.to_string());
        }

        let mut balance = 0;
        let mut split_index = 0;
        for (i, char) in s[1..s.len() - 1].chars().enumerate() {
            match char {
                '[' => balance += 1,
                ']' => balance -= 1,
                ',' => if balance == 0 {
                    split_index = i + 1;
                    break;
                },
                _ => {},
            }
             if split_index != 0 {
                break;
            }
        }
        let left = SnailNumber::from_str(&s[1..split_index])?;
        let right = SnailNumber::from_str(&s[split_index + 1..s.len() - 1])?;
        Ok(SnailNumber::Pair(Box::new(left), Box::new(right)))

    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines = io::BufReader::new(file).lines();
    let snail_numbers: Vec<SnailNumber> = lines
        .filter_map(Result::ok)
        .filter_map(|line| SnailNumber::from_str(&line).ok())
        .collect();

        if snail_numbers.is_empty() {
            println!("No snailfish numbers found in the file.");
            return Ok(());
        }

    let mut largest_magnitude = 0;
    for i in 0..snail_numbers.len() {
        for j in 0..snail_numbers.len() {
            if i == j {
                continue;
            }
            let sum1 = snail_numbers[i].clone().add(snail_numbers[j].clone()).magnitude();
            let sum2 = snail_numbers[j].clone().add(snail_numbers[i].clone()).magnitude();
            largest_magnitude = largest_magnitude.max(sum1);
            largest_magnitude = largest_magnitude.max(sum2);
        }
    }

    println!("{}", largest_magnitude);
    Ok(())
}
