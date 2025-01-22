
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::str::FromStr;

fn calculate(nums: &[i64], ops: &[Op]) -> i64 {
    let mut result = nums[0];
    for i in 0..ops.len() {
        match ops[i] {
            Op::Add => result += nums[i + 1],
            Op::Mul => result *= nums[i + 1],
            Op::Concat => {
                let right = nums[i + 1];
                let mut multiplier = 1;
                let mut temp = right;
                while temp > 0 {
                    multiplier *= 10;
                    temp /= 10;
                }
                result = result * multiplier + right;
            }
        }
    }
    result
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Add,
    Mul,
    Concat,
}

impl FromStr for Op {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Op::Add),
            "*" => Ok(Op::Mul),
            "||" => Ok(Op::Concat),
            _ => Err(()),
        }
    }
}

fn solve(line: &str) -> Option<i64> {
    let parts: Vec<&str> = line.split(": ").collect();
    let target = parts[0].parse::<i64>().unwrap();
    let nums: Vec<i64> = parts[1]
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();

    let num_ops = nums.len() - 1;

    for i in 0..(3i64.pow(num_ops as u32)) {
        let mut ops = Vec::new();
        let mut temp = i;
        for _ in 0..num_ops {
            match temp % 3 {
                0 => ops.push(Op::Add),
                1 => ops.push(Op::Mul),
                2 => ops.push(Op::Concat),
                _ => unreachable!(),
            }
            temp /= 3;
        }
        
        if calculate(&nums, &ops) == target {
            return Some(target);
        }
    }
    None
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    let mut total_sum = 0;
    for line in reader.lines() {
        let line = line?;
        if let Some(value) = solve(&line) {
            total_sum += value;
        }
    }

    println!("{}", total_sum);

    Ok(())
}
