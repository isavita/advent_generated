
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let parts: Vec<&str> = input.trim().split_whitespace().collect();
    let players: usize = parts[0].parse().unwrap();
    let last_marble: usize = parts[6].parse().unwrap();

    let mut circle = vec![0];
    let mut scores = vec![0; players];
    let mut current_player = 0;
    let mut current_marble_index = 0;

    for marble in 1..=last_marble {
        if marble % 23 == 0 {
            current_marble_index = (current_marble_index + circle.len() - 7) % circle.len();
            scores[current_player] += marble + circle.remove(current_marble_index);
        } else {
            current_marble_index = (current_marble_index + 2) % circle.len();
            circle.insert(current_marble_index, marble);
        }

        current_player = (current_player + 1) % players;
    }

    let max_score = scores.iter().max().unwrap();
    println!("{}", max_score);
}
