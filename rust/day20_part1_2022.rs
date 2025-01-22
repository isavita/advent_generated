
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let contents = fs::read_to_string("input.txt")?;
    let original_numbers: Vec<i64> = contents
        .lines()
        .filter_map(|line| line.parse().ok())
        .collect();
    let mut indexed_numbers: Vec<(usize, i64)> = original_numbers
        .iter()
        .enumerate()
        .map(|(i, &x)| (i, x))
        .collect();
    let len = indexed_numbers.len();

    for i in 0..len {
        let (old_index, value) = indexed_numbers
            .iter()
            .enumerate()
            .find(|(_, &(index, _))| index == i)
            .map(|(j, &(index, value))| (j, value))
            .unwrap();

        indexed_numbers.remove(old_index);

        let new_index = (old_index as i64 + value).rem_euclid(indexed_numbers.len() as i64) as usize;
        indexed_numbers.insert(new_index, (i, value));
    }

    let zero_index = indexed_numbers
        .iter()
        .position(|&(_, value)| value == 0)
        .unwrap();

    let grove_coordinates = [1000, 2000, 3000]
        .iter()
        .map(|offset| indexed_numbers[(zero_index + offset) % len].1)
        .sum::<i64>();

    println!("{}", grove_coordinates);

    Ok(())
}
