
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let groups: Vec<&str> = input.trim().split("\n\n").collect();

    let part1: usize = groups.iter().map(|group| {
        let mut answers = [false; 26];
        group.split_whitespace().for_each(|person| {
            person.chars().for_each(|c| {
                answers[(c as u8 - b'a') as usize] = true;
            });
        });
        answers.iter().filter(|&&x| x).count()
    }).sum();

    let part2: usize = groups.iter().map(|group| {
        let mut answers = [0; 26];
        let num_people = group.split_whitespace().count();
        group.split_whitespace().for_each(|person| {
            person.chars().for_each(|c| {
                answers[(c as u8 - b'a') as usize] += 1;
            });
        });
        answers.iter().filter(|&&x| x == num_people).count()
    }).sum();

    println!("{}", part1);
    println!("{}", part2);
}
