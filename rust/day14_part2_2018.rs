
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap().trim().to_string();

    let mut scoreboard = vec![3, 7];
    let (mut elf1, mut elf2) = (0, 1);
    let input_len = input.len();
    let mut input_sequence = Vec::new();

    for i in input.chars() {
        input_sequence.push(i.to_digit(10).unwrap() as usize);
    }

    loop {
        let new_score = scoreboard[elf1] + scoreboard[elf2];
        if new_score >= 10 {
            scoreboard.push(new_score / 10);
            if check_sequence(&scoreboard, &input_sequence) {
                break;
            }
        }
        scoreboard.push(new_score % 10);
        if check_sequence(&scoreboard, &input_sequence) {
            break;
        }

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.len();
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.len();
    }

    println!("{}", scoreboard.len() - input_len);
}

fn check_sequence(scoreboard: &Vec<usize>, sequence: &Vec<usize>) -> bool {
    if scoreboard.len() < sequence.len() {
        return false;
    }
    let start = scoreboard.len() - sequence.len();
    for (i, &v) in sequence.iter().enumerate() {
        if scoreboard[start + i] != v {
            return false;
        }
    }
    true
}
