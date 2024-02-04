use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read input file");
    let elves: Vec<Vec<i32>> = input.trim().split("\n\n").map(|group| group.lines().map(|x| x.parse::<i32>().unwrap()).collect()).collect();

    let total_calories: Vec<i32> = elves.iter().map(|elf| elf.iter().sum()).collect();
    let max_calories = total_calories.iter().max().unwrap();

    println!("{}", max_calories);

    let mut sorted_calories = total_calories.clone();
    sorted_calories.sort_by(|a, b| b.cmp(a));

    let top_three_calories: i32 = sorted_calories.iter().take(3).sum();

    println!("{}", top_three_calories);
}