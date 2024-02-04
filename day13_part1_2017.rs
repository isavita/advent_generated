
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let layers: Vec<(usize, usize)> = input.lines().map(|line| {
        let parts: Vec<usize> = line.split(": ").map(|x| x.parse().unwrap()).collect();
        (parts[0], parts[1])
    }).collect();

    let severity: usize = layers.iter().filter_map(|&(depth, range)| {
        if depth % (2 * (range - 1)) == 0 {
            Some(depth * range)
        } else {
            None
        }
    }).sum();

    println!("{}", severity);
}
