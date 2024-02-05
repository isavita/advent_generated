use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = input.trim().split('\n').collect();
    
    let mut gen_a: u64 = lines[0].split_whitespace().last().unwrap().parse().unwrap();
    let mut gen_b: u64 = lines[1].split_whitespace().last().unwrap().parse().unwrap();
    
    let factor_a: u64 = 16807;
    let factor_b: u64 = 48271;
    let divisor: u64 = 2147483647;
    
    let mut count = 0;
    
    for _ in 0..40_000_000 {
        gen_a = (gen_a * factor_a) % divisor;
        gen_b = (gen_b * factor_b) % divisor;
        
        if gen_a as u16 == gen_b as u16 {
            count += 1;
        }
    }
    
    println!("{}", count);
}