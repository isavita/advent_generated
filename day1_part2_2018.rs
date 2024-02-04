use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let changes: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();

    let mut frequency = 0;
    let mut frequencies = std::collections::HashSet::new();
    frequencies.insert(frequency);

    let result = changes
        .iter()
        .cycle()
        .find_map(|&change| {
            frequency += change;
            if !frequencies.insert(frequency) {
                Some(frequency)
            } else {
                None
            }
        })
        .unwrap();

    println!("{}", result);
}