
use std::collections::HashSet;
use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt").expect("Error reading input file");
    let distances = read_and_parse_input(&contents);

    let locations = get_unique_locations(&distances);
    let min_distance = find_shortest_route(&locations, &distances);
    println!("{}", min_distance);
}

fn read_and_parse_input(input: &str) -> std::collections::HashMap<String, std::collections::HashMap<String, i32>> {
    let mut distances = std::collections::HashMap::new();
    for line in input.lines() {
        let parts: Vec<&str> = line.split(" ").collect();
        if parts.len() != 5 {
            continue;
        }

        let from = parts[0].to_string();
        let to = parts[2].to_string();
        let dist = parts[4].parse().unwrap();

        distances.entry(from.clone()).or_insert(std::collections::HashMap::new()).insert(to.clone(), dist);
        distances.entry(to).or_insert(std::collections::HashMap::new()).insert(from, dist);
    }

    distances
}

fn get_unique_locations(distances: &std::collections::HashMap<String, std::collections::HashMap<String, i32>>) -> Vec<String> {
    let mut location_set = HashSet::new();
    for (from, _) in distances {
        location_set.insert(from.clone());
        for (to, _) in distances.get(from).unwrap() {
            location_set.insert(to.clone());
        }
    }

    location_set.into_iter().collect()
}

fn find_shortest_route(locations: &Vec<String>, distances: &std::collections::HashMap<String, std::collections::HashMap<String, i32>>) -> i32 {
    let mut min_distance = -1;
    permute(&locations, 0, &mut min_distance, &distances);
    min_distance
}

fn permute(arr: &Vec<String>, i: usize, min_distance: &mut i32, distances: &std::collections::HashMap<String, std::collections::HashMap<String, i32>>) {
    if i > arr.len() {
        return;
    }
    if i == arr.len() {
        let dist = calculate_route_distance(&arr, &distances);
        if *min_distance == -1 || dist < *min_distance {
            *min_distance = dist;
        }
        return;
    }
    for j in i..arr.len() {
        let mut arr = arr.clone();
        arr.swap(i, j);
        permute(&arr, i + 1, min_distance, &distances);
    }
}

fn calculate_route_distance(route: &Vec<String>, distances: &std::collections::HashMap<String, std::collections::HashMap<String, i32>>) -> i32 {
    let mut sum = 0;
    for i in 0..route.len() - 1 {
        sum += distances.get(&route[i]).unwrap().get(&route[i + 1]).unwrap();
    }
    sum
}
