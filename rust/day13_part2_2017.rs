
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let layers: Vec<(usize, usize)> = input
        .lines()
        .map(|line| {
            let parts: Vec<usize> = line.split(": ").map(|x| x.parse().unwrap()).collect();
            (parts[0], parts[1])
        })
        .collect();

    let severity: usize = layers
        .iter()
        .filter(|&&(depth, range)| depth % (2 * (range - 1)) == 0)
        .map(|&(depth, range)| depth * range)
        .sum();

    println!("{}", severity);

    let delay = (0..)
        .find(|delay| layers.iter().all(|&(depth, range)| (depth + delay) % (2 * (range - 1)) != 0))
        .unwrap();

    println!("{}", delay);
}
