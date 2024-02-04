
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let particles: Vec<Vec<i64>> = input.lines()
        .map(|line| line.split(", ")
            .flat_map(|s| s[3..s.len()-1].split(",").map(|num| num.parse::<i64>().unwrap()).collect::<Vec<i64>>())
            .collect())
        .collect();

    let closest_particle = particles.iter()
        .enumerate()
        .min_by_key(|&(_, particle)| particle[6].abs() + particle[7].abs() + particle[8].abs())
        .unwrap().0;

    println!("{}", closest_particle);
}
