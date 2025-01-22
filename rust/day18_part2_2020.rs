
use std::fs;

fn evaluate_expression_part1(expression: &str) -> i64 {
    let mut stack: Vec<i64> = Vec::new();
    let mut operator_stack: Vec<char> = Vec::new();
    let mut chars = expression.chars().filter(|c| !c.is_whitespace()).peekable();

    while let Some(c) = chars.next() {
        match c {
            '(' => operator_stack.push(c),
            ')' => {
                while let Some(op) = operator_stack.pop() {
                    if op == '(' {
                        break;
                    }
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(apply_operator(left, right, op));
                }
            }
            '+' | '*' => {
                while let Some(op) = operator_stack.last() {
                    if *op != '(' {
                        let right = stack.pop().unwrap();
                        let left = stack.pop().unwrap();
                        let op = operator_stack.pop().unwrap();
                        stack.push(apply_operator(left, right, op));
                    } else {
                        break;
                    }
                }
                operator_stack.push(c);
            }
            _ if c.is_digit(10) => {
                let mut num_str = String::new();
                num_str.push(c);
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_digit(10) {
                         num_str.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                stack.push(num_str.parse::<i64>().unwrap());
            }
            _ => (),
        }
    }

    while let Some(op) = operator_stack.pop() {
        let right = stack.pop().unwrap();
        let left = stack.pop().unwrap();
        stack.push(apply_operator(left, right, op));
    }

    stack.pop().unwrap()
}


fn apply_operator(left: i64, right: i64, operator: char) -> i64 {
    match operator {
        '+' => left + right,
        '*' => left * right,
        _ => panic!("Invalid operator"),
    }
}


fn evaluate_expression_part2(expression: &str) -> i64 {
    let mut stack: Vec<i64> = Vec::new();
    let mut operator_stack: Vec<char> = Vec::new();
    let mut chars = expression.chars().filter(|c| !c.is_whitespace()).peekable();

     while let Some(c) = chars.next() {
        match c {
            '(' => operator_stack.push(c),
            ')' => {
                while let Some(op) = operator_stack.pop() {
                    if op == '(' {
                        break;
                    }
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                     stack.push(apply_operator_part2(left, right, op));
                }
            }
             '+'  => {
                while let Some(op) = operator_stack.last() {
                    if *op == '+' {
                        let right = stack.pop().unwrap();
                        let left = stack.pop().unwrap();
                        let op = operator_stack.pop().unwrap();
                        stack.push(apply_operator_part2(left, right, op));
                    } else {
                        break;
                    }
                }
                 operator_stack.push(c);
            }
             '*' => {
                while let Some(op) = operator_stack.last() {
                  if  *op == '+' || *op == '*' {
                       let right = stack.pop().unwrap();
                       let left = stack.pop().unwrap();
                       let op = operator_stack.pop().unwrap();
                       stack.push(apply_operator_part2(left, right, op));
                  } else {
                    break;
                  }
                }
                operator_stack.push(c);
            }
            _ if c.is_digit(10) => {
                let mut num_str = String::new();
                num_str.push(c);
                while let Some(&next_c) = chars.peek() {
                    if next_c.is_digit(10) {
                         num_str.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                stack.push(num_str.parse::<i64>().unwrap());
            }
            _ => (),
        }
    }

    while let Some(op) = operator_stack.pop() {
        let right = stack.pop().unwrap();
        let left = stack.pop().unwrap();
        stack.push(apply_operator_part2(left, right, op));
    }

    stack.pop().unwrap()
}

fn apply_operator_part2(left: i64, right: i64, operator: char) -> i64 {
    match operator {
        '+' => left + right,
        '*' => left * right,
        _ => panic!("Invalid operator"),
    }
}

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut sum_part1 = 0;
    let mut sum_part2 = 0;

    for line in contents.lines() {
       sum_part1 += evaluate_expression_part1(line);
       sum_part2 += evaluate_expression_part2(line);
    }

    println!("Part 1 sum: {}", sum_part1);
     println!("Part 2 sum: {}", sum_part2);
}
