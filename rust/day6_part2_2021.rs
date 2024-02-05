
use std::fs;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut lantern_fish_counts = [0; 9];

    for line in content.lines() {
        let fish_ages: Vec<&str> = line.split(",").collect();
        for age in fish_ages {
            if let Ok(age_count) = age.parse::<usize>() {
                lantern_fish_counts[age_count] += 1;
            }
        }
    }

    for _ in 0..256 {
        let new_lantern_fish = lantern_fish_counts[0];
        for j in 0..lantern_fish_counts.len()-1 {
            lantern_fish_counts[j] = lantern_fish_counts[j+1];
        }
        lantern_fish_counts[6] += new_lantern_fish;
        lantern_fish_counts[8] = new_lantern_fish;
    }

    println!("{}", sum(&lantern_fish_counts));
}

fn sum(numbers: &[usize; 9]) -> usize {
    numbers.iter().sum()
}
