use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let expenses: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();

    for i in 0..expenses.len() {
        for j in i+1..expenses.len() {
            if expenses[i] + expenses[j] == 2020 {
                println!("{}", expenses[i] * expenses[j]);
            }
        }
    }

    for i in 0..expenses.len() {
        for j in i+1..expenses.len() {
            for k in j+1..expenses.len() {
                if expenses[i] + expenses[j] + expenses[k] == 2020 {
                    println!("{}", expenses[i] * expenses[j] * expenses[k]);
                }
            }
        }
    }
}