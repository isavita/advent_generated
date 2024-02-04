
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let mut cups: Vec<usize> = input.chars().map(|c| c.to_digit(10).unwrap() as usize).collect();

    let max_label = *cups.iter().max().unwrap();
    let min_label = *cups.iter().min().unwrap();

    let mut current_cup = cups[0];
    for _ in 0..100 {
        let current_cup_index = cups.iter().position(|&x| x == current_cup).unwrap();
        let picked_up: Vec<usize> = (1..=3).map(|i| cups[(current_cup_index + i) % cups.len()]).collect();
        for cup in &picked_up {
            cups.retain(|&x| x != *cup);
        }

        let mut destination_cup = current_cup - 1;
        while picked_up.contains(&destination_cup) || destination_cup < min_label {
            if destination_cup < min_label {
                destination_cup = max_label;
            } else {
                destination_cup -= 1;
            }
        }

        let destination_cup_index = cups.iter().position(|&x| x == destination_cup).unwrap();
        for (i, cup) in picked_up.iter().enumerate() {
            cups.insert(destination_cup_index + 1 + i, *cup);
        }

        current_cup = cups[(cups.iter().position(|&x| x == current_cup).unwrap() + 1) % cups.len()];
    }

    let one_index = cups.iter().position(|&x| x == 1).unwrap();
    let result: String = cups.iter().cycle().skip(one_index + 1).take(cups.len() - 1).map(|x| x.to_string()).collect();
    println!("{}", result);
}
