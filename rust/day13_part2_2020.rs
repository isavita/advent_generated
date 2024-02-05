
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.trim().split('\n').collect();
    let buses: Vec<(usize, usize)> = lines[1]
        .split(',')
        .enumerate()
        .filter(|&(_, id)| id != "x")
        .map(|(offset, id)| (offset, id.parse().unwrap()))
        .collect();

    let mut timestamp = 0;
    let mut step = 1;

    for (offset, id) in buses {
        while (timestamp + offset) % id != 0 {
            timestamp += step;
        }
        step *= id;
    }

    println!("{}", timestamp);
}
