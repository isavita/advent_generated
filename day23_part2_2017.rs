use std::fs;

fn is_prime(n: i32) -> bool {
    for i in 2..=(n as f64).sqrt() as i32 {
        if n % i == 0 {
            return false;
        }
    }
    true
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = input.trim().split('\n').collect();

    let b: i32 = 57 * 100 + 100000;
    let c: i32 = b + 17000;
    let mut h: i32 = 0;

    for x in (b..=c).step_by(17) {
        if !is_prime(x) {
            h += 1;
        }
    }

    println!("{}", h);
}