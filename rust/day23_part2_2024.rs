
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{self, BufRead},
};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
    let mut nodes_set: HashSet<String> = HashSet::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split('-').collect();
        if parts.len() != 2 {
            continue;
        }
        let (a, b) = (parts[0].to_string(), parts[1].to_string());
        graph.entry(a.clone()).or_insert(HashSet::new()).insert(b.clone());
        graph.entry(b.clone()).or_insert(HashSet::new()).insert(a.clone());
        nodes_set.insert(a);
        nodes_set.insert(b);
    }

    let all_nodes: Vec<String> = nodes_set.into_iter().collect();
    let mut best_clique: Vec<String> = Vec::new();

    bron_kerbosch(&mut best_clique, &graph, Vec::new(), all_nodes, Vec::new());
    best_clique.sort();
    println!("{}", best_clique.join(","));
    Ok(())
}

fn bron_kerbosch(
    best_clique: &mut Vec<String>,
    graph: &HashMap<String, HashSet<String>>,
    r: Vec<String>,
    mut p: Vec<String>,
    mut x: Vec<String>,
) {
    if p.is_empty() && x.is_empty() {
        if r.len() > best_clique.len() {
            *best_clique = r;
        }
        return;
    }
    let p_copy = p.clone();
    for v in p_copy {
        let neighbors: &HashSet<String> = graph.get(&v).unwrap();
        let new_r = {
            let mut temp_r = r.clone();
            temp_r.push(v.clone());
            temp_r
        };
        let new_p: Vec<String> = p
            .iter()
            .filter(|node| neighbors.contains(*node))
            .cloned()
            .collect();
        let new_x: Vec<String> = x
            .iter()
            .filter(|node| neighbors.contains(*node))
            .cloned()
            .collect();
        bron_kerbosch(best_clique, graph, new_r, new_p, new_x);
         p.retain(|node| node != &v);
         x.push(v);
    }
}
