use std::collections::HashMap;
use std::fs;

fn count_paths(
    cur: &str,
    tgt: &str,
    adj: &HashMap<String, Vec<String>>,
    memo: &mut HashMap<(String, String), u64>,
) -> u64 {
    if cur == tgt {
        return 1;
    }
    let key = (cur.to_string(), tgt.to_string());
    if let Some(&v) = memo.get(&key) {
        return v;
    }
    let mut total = 0;
    if let Some(neigh) = adj.get(cur) {
        for nxt in neigh {
            total += count_paths(nxt, tgt, adj, memo);
        }
    }
    memo.insert(key, total);
    total
}

fn main() {
    let data = match fs::read_to_string("input.txt") {
        Ok(s) => s,
        Err(_) => return,
    };
    let mut adj: HashMap<String, Vec<String>> = HashMap::new();
    for line in data.lines() {
        let line = line.trim();
        if line.is_empty() || !line.contains(':') {
            continue;
        }
        let parts: Vec<&str> = line.splitn(2, ':').collect();
        let node = parts[0].trim().to_string();
        let targets = parts[1].trim().split_whitespace().map(|s| s.to_string()).collect::<Vec<_>>();
        adj.entry(node).or_default().extend(targets);
    }
    let mut memo = HashMap::new();
    let p1 = count_paths("svr", "dac", &adj, &mut memo)
        * count_paths("dac", "fft", &adj, &mut memo)
        * count_paths("fft", "out", &adj, &mut memo);
    let p2 = count_paths("svr", "fft", &adj, &mut memo)
        * count_paths("fft", "dac", &adj, &mut memo)
        * count_paths("dac", "out", &adj, &mut memo);
    println!("{}", p1 + p2);
}