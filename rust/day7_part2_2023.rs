
use std::{
    cmp::Ordering,
    collections::HashMap,
    fs::read_to_string,
};

fn main() {
    let input = read_to_string("input.txt").expect("File not found");
    let mut hands: Vec<(&str, i64)> = Vec::new();
    for line in input.lines() {
        if line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split_whitespace().collect();
        hands.push((parts[0], parts[1].parse().unwrap()));
    }
    let mut matches: Vec<Vec<(&str, i64)>> = vec![Vec::new(); 7];
    let value_map = HashMap::from([
        ('J', 1),
        ('2', 2),
        ('3', 3),
        ('4', 4),
        ('5', 5),
        ('6', 6),
        ('7', 7),
        ('8', 8),
        ('9', 9),
        ('T', 10),
        ('Q', 11),
        ('K', 12),
        ('A', 13),
    ]);

    for (cards, bid) in hands {
        let mut counts: HashMap<char, i32> = HashMap::new();
        for card in cards.chars() {
            *counts.entry(card).or_insert(0) += 1;
        }
        let mut j_count = 0;
         if let Some(&j) = counts.get(&'J'){
            j_count = j;
        }
        if j_count > 0 {
            let mut high_v = 0;
            let mut high_key = 'J';
             for (&k, &v) in counts.iter() {
                if k != 'J' {
                   if v > high_v {
                        high_key = k;
                        high_v = v;
                    }else if v == high_v && value_map[&k] > value_map[&high_key] {
                        high_key = k
                    }
                }
             }
            if high_key != 'J' {
                *counts.get_mut(&high_key).unwrap() += j_count;
                counts.remove(&'J');
            }
        }
         let value = counts.values().fold(1, |acc, x| acc * x);
         let index = match value {
            1 => 6,
            2 => 5,
            3 => 3,
            4 => if counts.len() == 2 {1} else {4},
            5 => 0,
            6 => 2,
            _ => panic!("Unexpected Value"),
         };
        matches[index].push((cards, bid));
    }
     let mut converted_matches: Vec<(i64, i64)> = Vec::new();
     for mut x in matches {
        x.sort_by(|(a,_), (b,_)| {
            let a_val = a.chars().map(|c| match c {
                'A' => 'E',
                'T' => 'A',
                'J' => '1',
                'Q' => 'C',
                'K' => 'D',
                 _ => c
            }).collect::<String>();
            let b_val = b.chars().map(|c| match c {
                'A' => 'E',
                'T' => 'A',
                'J' => '1',
                'Q' => 'C',
                'K' => 'D',
                 _ => c
            }).collect::<String>();
            
            i64::from_str_radix(&b_val, 16).unwrap().cmp(&i64::from_str_radix(&a_val, 16).unwrap())
         });
        for (cards, bid) in x {
             let val = cards.chars().map(|c| match c {
                'A' => 'E',
                'T' => 'A',
                'J' => '1',
                'Q' => 'C',
                'K' => 'D',
                 _ => c
            }).collect::<String>();
                converted_matches.push((i64::from_str_radix(&val, 16).unwrap(), bid));
        }
     }

     let total: i64 = converted_matches.iter().enumerate().fold(0, |acc, (idx, (_, bid))| acc + (bid * (converted_matches.len() as i64 - idx as i64)));
     println!("{}", total);
}
