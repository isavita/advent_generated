
use std::collections::HashMap;
use std::fs;

struct Program {
    weight: i32,
    holds: Vec<String>,
}

fn dfs(name: &str, programs: &HashMap<String, Program>) -> (i32, bool) {
    let program = &programs[name];
    let mut total_weight = program.weight;
    let mut weights = HashMap::new();

    for child in &program.holds {
        let (weight, balanced) = dfs(child, programs);
        if !balanced {
            return (0, false);
        }
        total_weight += weight;
        *weights.entry(weight).or_insert(0) += 1;
    }

    // Check for unbalance
    for (w1, c1) in &weights {
        for (w2, c2) in &weights {
            if w1 != w2 && c1 < c2 {
                let mut unbalanced_program = String::new();
                for child in &program.holds {
                    let (child_weight, _) = dfs(child, programs);
                    if child_weight == *w1 {
                        unbalanced_program = child.to_string();
                        break;
                    }
                }
                println!("{}", programs[&unbalanced_program].weight + (w2 - w1));
                return (0, false);
            }
        }
    }
    (total_weight, true)
}

fn main() {
    let data = fs::read_to_string("input.txt")
        .expect("File reading error");

    let lines: Vec<&str> = data.trim().lines().collect();

    let mut programs: HashMap<String, Program> = HashMap::new();

    for line in lines {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let name = parts[0].to_string();
        let weight: i32 = parts[1].trim_matches(|p| p == '(' || p == ')').parse().expect("Invalid weight");

        let holds = if parts.len() > 3 {
            parts[3..].iter().map(|s| s.trim_matches(',').to_string()).collect()
        } else {
            Vec::new()
        };

        programs.insert(name, Program { weight, holds });
    }

    let root = "dtacyn".to_string(); // Replace this with the root found in Part One

    dfs(&root, &programs);
}
