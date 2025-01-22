
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::str::FromStr;

fn trim_leading_zeros(s: &str) -> &str {
    let mut i = 0;
    while i < s.len() - 1 && s.as_bytes()[i] == b'0' {
        i += 1;
    }
    &s[i..]
}

fn split_stone(s: &str) -> (&str, &str) {
    let mid = s.len() / 2;
    let left = trim_leading_zeros(&s[..mid]);
    let right = trim_leading_zeros(&s[mid..]);
    (if left.is_empty() { "0" } else { left }, if right.is_empty() { "0" } else { right })
}


fn multiply_by_2024(s: &str) -> String {
    let num: Vec<u32> = s.chars().map(|c| c.to_digit(10).unwrap()).collect();
    let multiplier: [u32; 4] = [2, 0, 2, 4];
    let mut result = vec![0; num.len() + multiplier.len()];

    for i in (0..num.len()).rev() {
        let mut carry = 0;
        for j in (0..multiplier.len()).rev() {
            let product = num[i] * multiplier[j] + result[i + j + 1] + carry;
            result[i + j + 1] = product % 10;
            carry = product / 10;
        }
        result[i] += carry;
    }
    let start = result.iter().position(|&x| x != 0).unwrap_or(result.len() - 1);

    result[start..].iter().map(|&x| char::from_digit(x, 10).unwrap()).collect()
}


fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);
    let line = reader.lines().next().ok_or(io::Error::new(io::ErrorKind::UnexpectedEof, "Input file is empty"))??;

    let stones_str: Vec<&str> = line.split_whitespace().collect();

    let mut stones_map: HashMap<String, i64> = HashMap::new();
    for &s in stones_str.iter() {
        *stones_map.entry(s.to_string()).or_insert(0) += 1;
    }

    const STEPS: usize = 75;
    for _ in 0..STEPS {
         let mut new_stones_map: HashMap<String, i64> = HashMap::new();
        for (stone, count) in stones_map {
            if stone == "0" {
                 *new_stones_map.entry("1".to_string()).or_insert(0) += count;
            } else if stone.len() % 2 == 0 {
                let (left, right) = split_stone(&stone);
                *new_stones_map.entry(left.to_string()).or_insert(0) += count;
                *new_stones_map.entry(right.to_string()).or_insert(0) += count;
            } else {
                let new_stone = multiply_by_2024(&stone);
                *new_stones_map.entry(new_stone).or_insert(0) += count;
            }
        }
        stones_map = new_stones_map;
    }
    let total_stones: i64 = stones_map.values().sum();
    println!("{}", total_stones);
    Ok(())
}
