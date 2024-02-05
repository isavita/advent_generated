
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() -> io::Result<()> {
    let happiness_map = read_happiness_values("input.txt")?;
    let mut happiness_map = add_yourself(happiness_map);
    let guests = get_guest_list(&happiness_map);
    let max_happiness = calculate_optimal_arrangement(&guests, &happiness_map);
    println!("{}", max_happiness);
    Ok(())
}

fn read_happiness_values(filename: &str) -> io::Result<HashMap<String, HashMap<String, i32>>> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    let mut happiness_map = HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() < 11 {
            continue;
        }
        let from = parts[0].to_string();
        let to = parts[10][..parts[10].len()-1].to_string();
        let mut change: i32 = parts[3].parse().unwrap();
        if parts[2] == "lose" {
            change = -change;
        }

        happiness_map.entry(from).or_insert_with(HashMap::new).insert(to, change);
    }
    Ok(happiness_map)
}

fn add_yourself(happiness_map: HashMap<String, HashMap<String, i32>>) -> HashMap<String, HashMap<String, i32>> {
    let mut extended_map = happiness_map.clone();
    extended_map.insert("You".to_string(), HashMap::new());
    for guest in happiness_map.keys() {
        extended_map.get_mut(guest).unwrap().insert("You".to_string(), 0);
        extended_map.get_mut("You").unwrap().insert(guest.to_string(), 0);
    }
    extended_map
}

fn get_guest_list(happiness_map: &HashMap<String, HashMap<String, i32>>) -> Vec<String> {
    happiness_map.keys().cloned().collect()
}

fn calculate_optimal_arrangement(guests: &[String], happiness_map: &HashMap<String, HashMap<String, i32>>) -> i32 {
    let mut guests = guests.to_vec();
    let mut max_happiness = i32::MIN;
    permute(&mut guests, 0, &mut max_happiness, happiness_map);
    max_happiness
}

fn permute(arr: &mut [String], i: usize, max_happiness: &mut i32, happiness_map: &HashMap<String, HashMap<String, i32>>) {
    if i == arr.len() {
        let happiness = calculate_happiness(&arr, happiness_map);
        *max_happiness = (*max_happiness).max(happiness);
        return;
    }
    for j in i..arr.len() {
        arr.swap(i, j);
        permute(arr, i + 1, max_happiness, happiness_map);
        arr.swap(i, j);
    }
}

fn calculate_happiness(arrangement: &[String], happiness_map: &HashMap<String, HashMap<String, i32>>) -> i32 {
    let n = arrangement.len();
    let mut happiness = 0;
    for i in 0..n {
        let left = (i + n - 1) % n;
        let right = (i + 1) % n;
        happiness += happiness_map.get(&arrangement[i]).unwrap().get(&arrangement[left]).unwrap();
        happiness += happiness_map.get(&arrangement[i]).unwrap().get(&arrangement[right]).unwrap();
    }
    happiness
}
