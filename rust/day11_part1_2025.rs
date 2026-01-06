
use std::collections::HashMap;
use std::fs;

fn count_paths(
    cur: &str,
    target: &str,
    adj: &HashMap<String, Vec<String>>,
    memo: &mut HashMap<String, u64>,
) -> u64 {
    if cur == target {
        return 1;
    }
    if let Some(&v) = memo.get(cur) {
        return v;
    }
    let res = adj
        .get(cur)
        .map_or(0, |nei| nei.iter().map(|n| count_paths(n, target, adj, memo)).sum());
    memo.insert(cur.to_string(), res);
    res
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut adj: HashMap<String, Vec<String>> = HashMap::new();
    for line in input.lines().map(str::trim).filter(|l| !l.is_empty()) {
        if let Some((src, dst)) = line.split_once(':') {
            let src = src.trim().to_string();
            let dst = dst
                .trim()
                .split_whitespace()
                .map(|s| s.to_string())
                .collect::<Vec<_>>();
            adj.insert(src, dst);
        }
    }
    let mut memo = HashMap::new();
    let count = count_paths("you", "out", &adj, &mut memo);
    println!("{}", count);
}
