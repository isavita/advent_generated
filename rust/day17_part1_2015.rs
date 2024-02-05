
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let containers: Vec<i32> = input.lines().map(|x| x.parse().unwrap()).collect();
    
    let mut count = 0;
    for i in 1..(1 << containers.len()) {
        let mut sum = 0;
        for j in 0..containers.len() {
            if (i >> j) & 1 == 1 {
                sum += containers[j];
            }
        }
        if sum == 150 {
            count += 1;
        }
    }
    
    println!("{}", count);
}
