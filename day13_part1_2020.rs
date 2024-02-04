
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let mut lines = input.lines();
    let timestamp: i32 = lines.next().unwrap().parse().unwrap();
    let buses: Vec<(i32, i32)> = lines.next().unwrap().split(',')
        .enumerate()
        .filter(|&(_, id)| id != "x")
        .map(|(i, id)| (i as i32, id.parse().unwrap()))
        .collect();
    
    let (bus_id, wait_time) = buses.iter()
        .map(|&(_, id)| (id, id - (timestamp % id)))
        .min_by_key(|&(_, wait)| wait)
        .unwrap();
    
    println!("{}", bus_id * wait_time);
}
