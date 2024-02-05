
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();
    let cups: Vec<usize> = input.chars().map(|c| c.to_digit(10).unwrap() as usize).collect();

    let result = play_game(&cups, 100);
    println!("{}", result);

    let million_cups: Vec<usize> = cups.iter().cloned().chain(10..=1_000_000).collect();
    let result_part_two = play_game(&million_cups, 10_000_000);
    println!("{}", result_part_two);
}

fn play_game(cups: &Vec<usize>, moves: usize) -> usize {
    let max_cup = *cups.iter().max().unwrap();
    let mut next_cup = vec![0; max_cup + 1];

    for i in 0..cups.len() - 1 {
        next_cup[cups[i]] = cups[i + 1];
    }
    next_cup[cups[cups.len() - 1]] = cups[0];

    let mut current_cup = cups[0];
    for _ in 0..moves {
        let pick_up_start = next_cup[current_cup];
        let pick_up_mid = next_cup[pick_up_start];
        let pick_up_end = next_cup[pick_up_mid];
        let mut destination = current_cup - 1;
        while destination == 0 || destination == pick_up_start || destination == pick_up_mid || destination == pick_up_end {
            if destination == 0 {
                destination = max_cup;
            } else {
                destination -= 1;
            }
        }

        next_cup[current_cup] = next_cup[pick_up_end];
        let temp = next_cup[destination];
        next_cup[destination] = pick_up_start;
        next_cup[pick_up_end] = temp;

        current_cup = next_cup[current_cup];
    }

    let cup_1_next = next_cup[1];
    let cup_2_next = next_cup[cup_1_next];

    cup_1_next * cup_2_next
}
