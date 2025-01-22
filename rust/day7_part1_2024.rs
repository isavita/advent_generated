
use std::fs::File;
use std::io::{self, BufRead};

fn evaluate(nums: &[i64], ops: &[char]) -> i64 {
    let mut result = nums[0];
    for i in 0..ops.len() {
        match ops[i] {
            '+' => result += nums[i + 1],
            '*' => result *= nums[i + 1],
            _ => panic!("Invalid operator"),
        }
    }
    result
}

fn solve_equation(target: i64, nums: &[i64]) -> bool {
    if nums.len() == 1 {
        return nums[0] == target;
    }
    let num_ops = nums.len() - 1;
    for i in 0..(2i32.pow(num_ops as u32)) {
        let mut ops = Vec::new();
        let mut temp = i;
        for _ in 0..num_ops {
            if temp % 2 == 0 {
                ops.push('+');
            } else {
                ops.push('*');
            }
            temp /= 2;
        }
        if evaluate(nums, &ops) == target {
            return true;
        }
    }
    false
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut total_calibration_result = 0;

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(": ").collect();
        let target: i64 = parts[0].parse().unwrap();
        let nums: Vec<i64> = parts[1]
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();

        if solve_equation(target, &nums) {
            total_calibration_result += target;
        }
    }

    println!("{}", total_calibration_result);

    Ok(())
}
