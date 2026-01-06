
use std::fs::File;
use std::io::{BufRead, BufReader};

fn max_subseq(s: &str, k: usize) -> String {
    let n = s.len();
    let mut to_remove = n - k;
    let mut stack = Vec::with_capacity(n);
    for d in s.chars() {
        while to_remove > 0 && !stack.is_empty() && stack[stack.len() - 1] < d {
            stack.pop();
            to_remove -= 1;
        }
        stack.push(d);
    }
    stack.truncate(k);
    stack.into_iter().collect()
}

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut total = 0u128;
    let k = 12;

    for line in reader.lines() {
        let s = line.unwrap().trim().to_string();
        if s.len() >= k {
            let best = max_subseq(&s, k);
            total += best.parse::<u128>().unwrap();
        }
    }

    println!("{}", total);
}
