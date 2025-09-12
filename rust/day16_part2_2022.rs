
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

#[derive(Debug)]
struct Valve {
    name: String,
    flow_rate: i32,
    tunnels_to: Vec<usize>,
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(&path)?;
    let reader = io::BufReader::new(file);

    let mut valve_name_to_id: HashMap<String, usize> = HashMap::new();
    let mut all_valves: Vec<Valve> = Vec::new();
    let mut next_id = 0;

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 10 { continue; }

        let name = parts[1].to_string();
        let flow_rate_str = parts[4];
        let flow_rate = flow_rate_str[5..flow_rate_str.len()-1].parse::<i32>().unwrap_or(0);
        
        let current_id = *valve_name_to_id.entry(name.clone()).or_insert_with(|| {
            let id = next_id;
            next_id += 1;
            all_valves.push(Valve { name: name.clone(), flow_rate: 0, tunnels_to: Vec::new() });
            id
        });
        
        all_valves[current_id].flow_rate = flow_rate;
        
        for i in 9..parts.len() {
            let tunnel_name = parts[i].trim_end_matches(',').to_string();
            let tunnel_id = *valve_name_to_id.entry(tunnel_name.clone()).or_insert_with(|| {
                let id = next_id;
                next_id += 1;
                all_valves.push(Valve { name: tunnel_name, flow_rate: 0, tunnels_to: Vec::new() });
                id
            });
            all_valves[current_id].tunnels_to.push(tunnel_id);
        }
    }

    let start_valve_orig_id = valve_name_to_id["AA"];
    let mut important_valve_orig_ids: Vec<usize> = Vec::new();
    let mut original_id_to_important_idx: HashMap<usize, usize> = HashMap::new();
    let mut important_flow_rates: Vec<i32> = Vec::new();
    let mut k_important_valves = 0;

    for (id, valve) in all_valves.iter().enumerate() {
        if valve.flow_rate > 0 {
            important_valve_orig_ids.push(id);
            original_id_to_important_idx.insert(id, k_important_valves);
            important_flow_rates.push(valve.flow_rate);
            k_important_valves += 1;
        }
    }

    if !original_id_to_important_idx.contains_key(&start_valve_orig_id) {
        important_valve_orig_ids.push(start_valve_orig_id);
        original_id_to_important_idx.insert(start_valve_orig_id, k_important_valves);
        important_flow_rates.push(all_valves[start_valve_orig_id].flow_rate);
        k_important_valves += 1;
    }

    let start_important_idx = original_id_to_important_idx[&start_valve_orig_id];
    let mut dist_matrix: Vec<Vec<i32>> = vec![vec![i32::MAX; k_important_valves]; k_important_valves];

    for i in 0..k_important_valves {
        let start_node_orig_id = important_valve_orig_ids[i];
        let mut q = VecDeque::new();
        let mut distances: HashMap<usize, i32> = HashMap::new();
        q.push_back((start_node_orig_id, 0));
        distances.insert(start_node_orig_id, 0);

        while let Some((u_orig_id, dist)) = q.pop_front() {
            if let Some(&idx) = original_id_to_important_idx.get(&u_orig_id) {
                dist_matrix[i][idx] = dist_matrix[i][idx].min(dist);
            }

            for &v_orig_id in &all_valves[u_orig_id].tunnels_to {
                if !distances.contains_key(&v_orig_id) {
                    distances.insert(v_orig_id, dist + 1);
                    q.push_back((v_orig_id, dist + 1));
                }
            }
        }
    }

    let mut memo: Vec<Vec<Vec<i32>>> = vec![vec![vec![-1; 1 << k_important_valves]; 31]; k_important_valves];
    let part1 = dfs_part1(start_important_idx, 30, 0, k_important_valves, &important_flow_rates, &dist_matrix, &mut memo);
    println!("{}", part1);

    let mut memo_part2: Vec<Vec<Vec<i32>>> = vec![vec![vec![-1; 1 << k_important_valves]; 27]; k_important_valves];
    let mut max_pressure_for_mask_achieved: HashMap<usize, i32> = HashMap::new();
    dfs_part2(start_important_idx, 26, 0, 0, k_important_valves, &important_flow_rates, &dist_matrix, &mut memo_part2, &mut max_pressure_for_mask_achieved);

    let mut max_combined = 0;
    for (&mask1, &p1) in &max_pressure_for_mask_achieved {
        for (&mask2, &p2) in &max_pressure_for_mask_achieved {
            if mask1 & mask2 == 0 {
                max_combined = max_combined.max(p1 + p2);
            }
        }
    }
    println!("{}", max_combined);

    Ok(())
}

fn dfs_part1(u: usize, minutes_left: i32, opened_mask: usize, k: usize, flows: &[i32], dists: &[Vec<i32>], memo: &mut Vec<Vec<Vec<i32>>>) -> i32 {
    if minutes_left <= 0 { return 0; }
    if memo[u][minutes_left as usize][opened_mask] != -1 { return memo[u][minutes_left as usize][opened_mask]; }

    let mut max_pressure = 0;
    if opened_mask & (1 << u) == 0 && flows[u] > 0 {
        let new_time = minutes_left - 1;
        if new_time >= 0 {
            let pressure = flows[u] * new_time;
            max_pressure = max_pressure.max(pressure + dfs_part1(u, new_time, opened_mask | (1 << u), k, flows, dists, memo));
        }
    }

    for v in 0..k {
        if u == v { continue; }
        let cost = dists[u][v];
        let new_time = minutes_left - cost;
        if new_time >= 0 {
            max_pressure = max_pressure.max(dfs_part1(v, new_time, opened_mask, k, flows, dists, memo));
        }
    }

    memo[u][minutes_left as usize][opened_mask] = max_pressure;
    max_pressure
}

fn dfs_part2(u: usize, minutes_left: i32, opened_mask: usize, current_pressure: i32, k: usize, flows: &[i32], dists: &[Vec<i32>], memo: &mut Vec<Vec<Vec<i32>>>, max_map: &mut HashMap<usize, i32>) {
    *max_map.entry(opened_mask).or_insert(0) = (*max_map.get(&opened_mask).unwrap_or(&0)).max(current_pressure);
    if memo[u][minutes_left as usize][opened_mask] >= current_pressure { return; }
    memo[u][minutes_left as usize][opened_mask] = current_pressure;

    if opened_mask & (1 << u) == 0 && flows[u] > 0 {
        let new_time = minutes_left - 1;
        if new_time >= 0 {
            let pressure_gain = flows[u] * new_time;
            dfs_part2(u, new_time, opened_mask | (1 << u), current_pressure + pressure_gain, k, flows, dists, memo, max_map);
        }
    }

    for v in 0..k {
        if u == v { continue; }
        let cost = dists[u][v];
        let new_time = minutes_left - cost;
        if new_time >= 0 {
            dfs_part2(v, new_time, opened_mask, current_pressure, k, flows, dists, memo, max_map);
        }
    }
}
