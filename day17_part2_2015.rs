use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let containers: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();

    let mut count = 0;
    let mut min_count = containers.len();
    let mut min_ways = 0;

    for i in 1..(1 << containers.len()) {
        let mut sum = 0;
        let mut num_containers = 0;

        for j in 0..containers.len() {
            if i & (1 << j) != 0 {
                sum += containers[j];
                num_containers += 1;
            }
        }

        if sum == 150 {
            count += 1;
            if num_containers < min_count {
                min_count = num_containers;
                min_ways = 1;
            } else if num_containers == min_count {
                min_ways += 1;
            }
        }
    }

    println!("{}", count);
    println!("{}", min_ways);
}