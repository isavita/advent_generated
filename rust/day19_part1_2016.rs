
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let n: usize = input.trim().parse().unwrap();

    let mut result = 0;
    let mut i = 1;
    while i <= n {
        result = (result + 2) % i;
        i += 1;
    }

    println!("{}", result + 1);
}
