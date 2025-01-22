
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut adj_list: HashMap<String, HashSet<String>> = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split('-').collect();
        if parts.len() == 2 {
            let u = parts[0].to_string();
            let v = parts[1].to_string();

            adj_list.entry(u.clone()).or_insert_with(HashSet::new).insert(v.clone());
            adj_list.entry(v).or_insert_with(HashSet::new).insert(u);
        }
    }

    let mut count = 0;
    let nodes: Vec<String> = adj_list.keys().cloned().collect();
    for i in 0..nodes.len() {
        for j in i + 1..nodes.len() {
            for k in j + 1..nodes.len() {
                let u = &nodes[i];
                let v = &nodes[j];
                let w = &nodes[k];
                
                if adj_list.get(u).map_or(false, |neighbors| neighbors.contains(v) && neighbors.contains(w)) &&
                    adj_list.get(v).map_or(false, |neighbors| neighbors.contains(u) && neighbors.contains(w)) &&
                    adj_list.get(w).map_or(false, |neighbors| neighbors.contains(u) && neighbors.contains(v)) {
                    if u.starts_with('t') || v.starts_with('t') || w.starts_with('t'){
                       count += 1;
                    }
                }
            }
        }
    }
    println!("{}", count);
    Ok(())
}
