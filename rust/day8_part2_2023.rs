use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: i64, b: i64) -> i64 {
    (a * b).abs() / gcd(a, b)
}

fn lcm_slice(nums: &[i64]) -> i64 {
    let mut res = nums[0];
    for &num in &nums[1..] {
        res = lcm(res, num);
    }
    res
}

fn parse_input(lines: Vec<String>) -> (String, Vec<(String, [String; 2])>) {
    let instructions = lines[0].clone();
    let mut nodes = Vec::new();
    for line in &lines[2..] {
        let parts: Vec<_> = line.split(" = ").collect();
        let head = parts[0].to_string();
        let children_trim = parts[1].trim_matches(|c| c == '(' || c == ')');
        let children_parts: Vec<_> = children_trim.split(", ").collect();
        let children = [children_parts[0].to_string(), children_parts[1].to_string()];
        nodes.push((head, children));
    }
    (instructions, nodes)
}

fn solve(input: Vec<String>) -> i64 {
    let (instructions, nodes) = parse_input(input);
    let mut starts = Vec::new();
    for &(ref node, _) in &nodes {
        if node.ends_with("A") {
            starts.push(node.clone());
        }
    }
    let instructions_len = instructions.len() as i64;
    let mut steps = vec![0; starts.len()];
    for i in 0..starts.len() {
        let mut element = starts[i].clone();
        loop {
            let last_char = element.chars().last().unwrap();
            if last_char == 'Z' {
                break;
            }
            let instruction = instructions.chars().nth((steps[i] % instructions_len) as usize).unwrap();
            let node = nodes.iter().find(|&&(ref n, _)| n == &element).unwrap();
            element = if instruction == 'L' {
                node.1[0].clone()
            } else {
                node.1[1].clone()
            };
            steps[i] += 1;
        }
    }
    lcm_slice(&steps)
}

fn read_file<P>(filename: P) -> io::Result<Vec<String>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    let lines: Vec<String> = io::BufReader::new(file).lines().map(|l| l.unwrap()).collect();
    Ok(lines)
}

fn main() {
    let input = read_file("input.txt").unwrap();
    println!("{}", solve(input));
}