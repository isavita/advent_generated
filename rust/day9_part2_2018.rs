
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn play_marble_game(players: usize, last_marble: usize) -> usize {
    let mut scores = vec![0; players];
    let mut circle = VecDeque::with_capacity(last_marble + 1);
    circle.push_back(0);
    let mut current_player = 0;

    for marble in 1..=last_marble {
        if marble % 23 == 0 {
            scores[current_player] += marble;
            for _ in 0..7 {
                let removed = circle.pop_back().unwrap();
                circle.push_front(removed);
            }
            scores[current_player] += circle.pop_front().unwrap();
        } else {
            for _ in 0..2 {
                let front = circle.pop_front().unwrap();
                circle.push_back(front);
            }
            circle.push_front(marble);
        }
        current_player = (current_player + 1) % players;
    }

    *scores.iter().max().unwrap()
}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);
    let mut line = String::new();
    reader.lines().next().unwrap().unwrap_or_default().clone_into(&mut line);

    let parts: Vec<&str> = line.split_whitespace().collect();
    let players: usize = parts[0].parse().unwrap();
    let last_marble: usize = parts[6].parse().unwrap();

    let high_score = play_marble_game(players, last_marble);
    println!("Part 1 - High score: {}", high_score);

    let high_score_part2 = play_marble_game(players, last_marble * 100);
    println!("Part 2 - High score: {}", high_score_part2);
}
