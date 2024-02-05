fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let sum: i32 = input
        .lines()
        .map(|line| {
            let mass: i32 = line.parse().unwrap();
            mass / 3 - 2
        })
        .sum();
    
    println!("{}", sum);
}