
use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut total_points = 0;
    let mut card_counts: Vec<usize> = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split(':').collect();
        let numbers_part = parts[1].trim();
        let number_parts: Vec<&str> = numbers_part.split('|').collect();
        let winning_numbers_str = number_parts[0].trim();
        let my_numbers_str = number_parts[1].trim();

        let winning_numbers: HashSet<usize> = winning_numbers_str
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();

        let my_numbers: Vec<usize> = my_numbers_str
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();
        
        let matches = my_numbers.iter().filter(|num| winning_numbers.contains(num)).count();

        // Part 1: Calculate points
        if matches > 0 {
          total_points += 1 << (matches - 1);
        }
        
        card_counts.push(matches);
    }

    println!("Part 1 Total points: {}", total_points);


    // Part 2: calculate total cards won
    let mut total_cards = 0;
    let num_cards = card_counts.len();
    let mut card_multipliers: Vec<usize> = vec![1; num_cards];
    for i in 0..num_cards {
        total_cards += card_multipliers[i];
        let matches = card_counts[i];
        for j in 1..=matches {
            if i + j < num_cards {
              card_multipliers[i+j] += card_multipliers[i];
            }
        }

    }
    
    println!("Part 2 Total Cards: {}", total_cards);

    Ok(())
}
