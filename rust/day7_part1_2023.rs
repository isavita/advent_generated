
use std::{
    cmp::Ordering,
    collections::HashMap,
    fs::File,
    io::{self, BufRead},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Card {
    C2,
    C3,
    C4,
    C5,
    C6,
    C7,
    C8,
    C9,
    CT,
    CJ,
    CQ,
    CK,
    CA,
}

impl Card {
    fn from_char(c: char) -> Self {
        match c {
            '2' => Card::C2,
            '3' => Card::C3,
            '4' => Card::C4,
            '5' => Card::C5,
            '6' => Card::C6,
            '7' => Card::C7,
            '8' => Card::C8,
            '9' => Card::C9,
            'T' => Card::CT,
            'J' => Card::CJ,
            'Q' => Card::CQ,
            'K' => Card::CK,
            'A' => Card::CA,
            _ => panic!("Invalid card character: {}", c),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

#[derive(Debug, PartialEq, Eq)]
struct Hand {
    cards: Vec<Card>,
    bid: u32,
    hand_type: HandType,
}

impl Hand {
    fn new(cards: Vec<Card>, bid: u32) -> Self {
        let hand_type = Hand::determine_hand_type(&cards);
        Hand {
            cards,
            bid,
            hand_type,
        }
    }

    fn determine_hand_type(cards: &Vec<Card>) -> HandType {
        let mut counts = HashMap::new();
        for card in cards {
            *counts.entry(card).or_insert(0) += 1;
        }

        let mut counts_vec: Vec<u32> = counts.values().copied().collect();
        counts_vec.sort_by(|a, b| b.cmp(a));
        
        match counts_vec.as_slice() {
            [5] => HandType::FiveOfAKind,
            [4, 1] => HandType::FourOfAKind,
            [3, 2] => HandType::FullHouse,
            [3, 1, 1] => HandType::ThreeOfAKind,
            [2, 2, 1] => HandType::TwoPair,
            [2, 1, 1, 1] => HandType::OnePair,
            _ => HandType::HighCard,
        }
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.hand_type.cmp(&other.hand_type) {
            Ordering::Equal => {
                for (c1, c2) in self.cards.iter().zip(other.cards.iter()) {
                   let card_comparison = c1.cmp(c2);
                   if card_comparison != Ordering::Equal {
                        return card_comparison;
                   }
                }
                Ordering::Equal
            },
            other => other,
        }
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut hands: Vec<Hand> = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let cards: Vec<Card> = parts[0].chars().map(Card::from_char).collect();
        let bid: u32 = parts[1].parse().unwrap();
        hands.push(Hand::new(cards, bid));
    }

    hands.sort();

    let total_winnings: u64 = hands
        .iter()
        .enumerate()
        .map(|(rank, hand)| (rank as u64 + 1) * hand.bid as u64)
        .sum();

    println!("{}", total_winnings);

    Ok(())
}
