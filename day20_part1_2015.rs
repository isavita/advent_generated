use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let target: i32 = input.trim().parse().expect("Invalid input");

    let mut target = target / 10;
    let mut houses = vec![0; (target + 1) as usize];

    for elf in 1..=target {
        for house in (elf..=target).step_by(elf as usize) {
            houses[house as usize] += elf;
        }
    }

    for (house_number, presents) in houses.iter().enumerate() {
        if *presents >= target {
            println!("{}", house_number);
            break;
        }
    }
}