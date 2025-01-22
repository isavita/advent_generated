
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn solve_part1(num_elves: usize) -> usize {
    let mut elves: VecDeque<usize> = (1..=num_elves).collect();

    while elves.len() > 1 {
        let stealer = elves.pop_front().unwrap();
        elves.pop_front();
        elves.push_back(stealer);
    }

    elves[0]
}

fn solve_part2(num_elves: usize) -> usize {
    let mut left: VecDeque<usize> = (1..=num_elves / 2).collect();
    let mut right: VecDeque<usize> = (num_elves / 2 + 1..=num_elves).collect();

    while !left.is_empty() && !right.is_empty() {
        if left.len() > right.len() {
            left.pop_back();
        } else {
            right.pop_back();
        }

        right.push_front(left.pop_front().unwrap());
        left.push_back(right.pop_back().unwrap());
    }
    
    if left.is_empty() {
        right[0]
    } else {
        left[0]
    }

}

fn main() {
    let file = File::open("input.txt").expect("Failed to open input file");
    let reader = BufReader::new(file);

    let num_elves: usize = reader
        .lines()
        .next()
        .expect("Empty input file")
        .expect("Failed to read line")
        .parse()
        .expect("Invalid input: not a number");

    let part1_winner = solve_part1(num_elves);
    println!("{}", part1_winner);

    let part2_winner = solve_part2(num_elves);
    println!("{}", part2_winner);
}
