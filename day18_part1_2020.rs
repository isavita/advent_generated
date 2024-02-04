
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let sum: i64 = input
        .lines()
        .map(|line| evaluate_expression(line))
        .sum();
    
    println!("{}", sum);
}

fn evaluate_expression(expression: &str) -> i64 {
    let mut result = 0;
    let mut operator = '+';
    let mut i = 0;

    while i < expression.len() {
        let c = expression.chars().nth(i).unwrap();
        match c {
            '0'..='9' => {
                let num = c.to_digit(10).unwrap() as i64;
                match operator {
                    '+' => result += num,
                    '*' => result *= num,
                    _ => (),
                }
            }
            '+' | '*' => operator = c,
            '(' => {
                let mut count = 1;
                let mut j = i + 1;
                while count > 0 {
                    match expression.chars().nth(j).unwrap() {
                        '(' => count += 1,
                        ')' => count -= 1,
                        _ => (),
                    }
                    j += 1;
                }
                let sub_result = evaluate_expression(&expression[i + 1..j - 1]);
                match operator {
                    '+' => result += sub_result,
                    '*' => result *= sub_result,
                    _ => (),
                }
                i = j - 1;
            }
            _ => (),
        }
        i += 1;
    }

    result
}
