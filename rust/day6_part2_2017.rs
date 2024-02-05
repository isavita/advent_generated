
use std::fs;

fn main() {
    let data = fs::read_to_string("input.txt").expect("File reading error");
    let str_arr: Vec<&str> = data.trim().split_whitespace().collect();
    let mut banks: Vec<i32> = str_arr.iter().map(|&x| x.parse().unwrap()).collect();

    let mut seen = std::collections::HashMap::new();
    let mut cycles = 0;

    loop {
        let state = format!("{:?}", banks);

        if let Some(&prev_cycle) = seen.get(&state) {
            println!("The size of the loop is {}", cycles - prev_cycle);
            return;
        }
        seen.insert(state, cycles);

        let mut max_index = 0;
        for (i, &num) in banks.iter().enumerate().skip(1) {
            if num > banks[max_index] {
                max_index = i;
            }
        }

        let blocks = banks[max_index];
        banks[max_index] = 0;
        for i in 1..=blocks {
            let index = (max_index + i as usize) % banks.len();
            banks[index] += 1;
        }

        cycles += 1;
    }
}
