
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let starting_numbers: Vec<usize> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();
    
    let mut last_spoken: Vec<Option<usize>> = vec![None; 2020];
    
    for (i, &num) in starting_numbers.iter().enumerate() {
        last_spoken[num] = Some(i + 1);
    }
    
    let mut prev_num = *starting_numbers.last().unwrap();
    
    for i in starting_numbers.len()..2020 {
        let next_num = match last_spoken[prev_num] {
            Some(turn) => i - turn,
            None => 0,
        };
        
        last_spoken[prev_num] = Some(i);
        prev_num = next_num;
    }
    
    println!("{}", prev_num);
}
