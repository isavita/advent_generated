use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let target: usize = input.trim().parse().expect("Invalid input");

    let mut houses = vec![0; target / 10];

    for i in 1..(target / 10) {
        for j in 1..=50 {
            if i * j < target / 10 {
                houses[i * j] += i * 11;
            }
        }
    }

    for (i, &presents) in houses.iter().enumerate() {
        if presents >= target {
            println!("{}", i * 10);
            break;
        }
    }
}