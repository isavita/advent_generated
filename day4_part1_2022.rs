
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let pairs: Vec<&str> = input.trim().split('\n').collect();

    let mut count = 0;
    for pair in pairs {
        let sections: Vec<Vec<u32>> = pair.split(',').map(|s| {
            let range: Vec<u32> = s.split('-').map(|n| n.parse().unwrap()).collect();
            range
        }).collect();

        let first_contains_second = sections[0][0] <= sections[1][0] && sections[0][1] >= sections[1][1];
        let second_contains_first = sections[1][0] <= sections[0][0] && sections[1][1] >= sections[0][1];

        if first_contains_second || second_contains_first {
            count += 1;
        }
    }

    println!("{}", count);
}
