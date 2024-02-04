
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Error reading from file");

    println!("{}", some_assembly_required(&input));
}

fn some_assembly_required(input: &str) -> i32 {
    let mut wire_to_rule: std::collections::HashMap<String, String> = std::collections::HashMap::new();

    for inst in input.trim().split("\n") {
        let parts: Vec<&str> = inst.split(" -> ").collect();
        wire_to_rule.insert(parts[1].to_string(), parts[0].to_string());
    }

    let a_signal = memo_dfs(&wire_to_rule, "a", &mut std::collections::HashMap::new());

    wire_to_rule.insert("b".to_string(), a_signal.to_string());
    memo_dfs(&wire_to_rule, "a", &mut std::collections::HashMap::new())
}

fn memo_dfs(graph: &std::collections::HashMap<String, String>, entry: &str, memo: &mut std::collections::HashMap<String, i32>) -> i32 {
    if let Some(&memo_val) = memo.get(entry) {
        return memo_val;
    }

    if entry.chars().all(char::is_numeric) {
        return entry.parse().unwrap();
    }

    let source_rule = graph.get(entry).unwrap();
    let parts: Vec<&str> = source_rule.split(" ").collect();

    let result: i32;
    match parts.len() {
        1 => result = memo_dfs(graph, parts[0], memo),
        2 if parts[0] == "NOT" => {
            let start = memo_dfs(graph, parts[1], memo);
            result = std::u16::MAX as i32 ^ start;
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
