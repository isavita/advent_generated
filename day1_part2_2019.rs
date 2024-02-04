fn calculate_fuel(mass: i32) -> i32 {
    mass / 3 - 2
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let total_fuel: i32 = input.lines().map(|line| {
        let mass: i32 = line.parse().unwrap();
        let mut fuel = calculate_fuel(mass);
        let mut total = 0;
        while fuel > 0 {
            total += fuel;
            fuel = calculate_fuel(fuel);
        }
        total
    }).sum();

    println!("{}", total_fuel);
}