
use std::fs;

type Deck = Vec<i32>;

trait Copy {
    fn copy(&self, n: usize) -> Deck;
}

impl Copy for Deck {
    fn copy(&self, n: usize) -> Deck {
        self.iter().take(n).cloned().collect()
    }
}

trait Score {
    fn score(&self) -> i32;
}

impl Score for Deck {
    fn score(&self) -> i32 {
        self.iter().rev().enumerate().map(|(i, &card)| card * (i as i32 + 1)).sum()
    }
}

fn play_recursive_combat(mut player1: Deck, mut player2: Deck) -> (Deck, Deck) {
    let mut previous_rounds = std::collections::HashSet::new();
    while !player1.is_empty() && !player2.is_empty() {
        let round_key = format!("{:?}|{:?}", player1, player2);
        if !previous_rounds.insert(round_key) {
            return (player1, vec![]);
        }

        let card1 = player1.remove(0);
        let card2 = player2.remove(0);

        if player1.len() >= card1 as usize && player2.len() >= card2 as usize {
            let (sub_player1, _) = play_recursive_combat(player1.copy(card1 as usize), player2.copy(card2 as usize));
            if !sub_player1.is_empty() {
                player1.push(card1);
                player1.push(card2);
            } else {
                player2.push(card2);
                player2.push(card1);
            }
        } else {
            if card1 > card2 {
                player1.push(card1);
                player1.push(card2);
            } else {
                player2.push(card2);
                player2.push(card1);
            }
        }
    }
    (player1, player2)
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut player1_deck = vec![];
    let mut player2_deck = vec![];
    let mut current_deck = &mut player1_deck;

    for line in input.lines() {
        if line.is_empty() {
            current_deck = &mut player2_deck;
            continue;
        }
        if line.contains("Player") {
            continue;
        }
        let card: i32 = line.parse().unwrap();
        current_deck.push(card);
    }

    let (player1_deck, player2_deck) = play_recursive_combat(player1_deck, player2_deck);

    let winning_deck = if !player1_deck.is_empty() { player1_deck } else { player2_deck };

    println!("{}", winning_deck.score());
}
