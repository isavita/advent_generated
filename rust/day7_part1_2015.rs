
use std::collections::HashMap;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Error reading from file");
    println!("{}", some_assembly_required(&input));
}

fn some_assembly_required(input: &str) -> u16 {
    let mut wire_to_rule: HashMap<String, String> = HashMap::new();

    for inst in input.trim().split("\n") {
        let parts: Vec<&str> = inst.split(" -> ").collect();
        wire_to_rule.insert(parts[1].to_string(), parts[0].to_string());
    }

    memo_dfs(&wire_to_rule, "a", &mut HashMap::new())
}

fn memo_dfs(graph: &HashMap<String, String>, entry: &str, memo: &mut HashMap<String, u16>) -> u16 {
    if let Some(&memo_val) = memo.get(entry) {
        return memo_val;
    }

    if entry.chars().all(char::is_numeric) {
        return entry.parse().unwrap();
    }

    let source_rule = graph.get(entry).unwrap();
    let parts: Vec<&str> = source_rule.split(" ").collect();

    let result: u16;
    match parts.len() {
        1 => result = memo_dfs(graph, parts[0], memo),
        2 if parts[0] == "NOT" => {
            let start = memo_dfs(graph, parts[1], memo);
            result = !start;
        }
        3 => match parts[1] {
            "AND" => result = memo_dfs(graph, parts[0], memo) & memo_dfs(graph, parts[2], memo),
            "OR" => result = memo_dfs(graph, parts[0], memo) | memo_dfs(graph, parts[2], memo),
            "LSHIFT" => result = memo_dfs(graph, parts[0], memo) << memo_dfs(graph, parts[2], memo),
            "RSHIFT" => result = memo_dfs(graph, parts[0], memo) >> memo_dfs(graph, parts[2], memo),
            _ => panic!("Invalid operation"),
        },
        _ => panic!("Invalid rule"),
    }

    memo.insert(entry.to_string(), result);
    result
}
