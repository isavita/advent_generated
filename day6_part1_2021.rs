
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let lanternfish: Vec<usize> = input.trim().split(',').map(|x| x.parse().unwrap()).collect();

    let mut fish: Vec<usize> = lanternfish.clone();

    for _ in 0..80 {
        let mut new_fish: Vec<usize> = Vec::new();
        for &f in &fish {
            if f == 0 {
                new_fish.push(6);
                new_fish.push(8);
            } else {
                new_fish.push(f - 1);
            }
        }
        fish = new_fish;
    }

    println!("{}", fish.len());
}
