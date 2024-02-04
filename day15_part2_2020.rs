use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let starting_numbers: Vec<usize> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();

    let mut last_spoken = vec![0; 30_000_000];
    let mut turn = 1;
    let mut last_num = 0;

    for &num in &starting_numbers {
        last_spoken[num] = turn;
        turn += 1;
        last_num = num;
    }

    while turn <= 30_000_000 {
        let next_num = if last_spoken[last_num] == 0 {
            0
        } else {
            turn - 1 - last_spoken[last_num]
        };

        last_spoken[last_num] = turn - 1;
        last_num = next_num;
        turn += 1;
    }

    println!("{}", last_num);
}