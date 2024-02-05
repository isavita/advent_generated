use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let recipes = input.trim().parse::<usize>().unwrap();

    let mut scoreboard = vec![3, 7];
    let mut elf1 = 0;
    let mut elf2 = 1;

    while scoreboard.len() < recipes + 10 {
        let sum = scoreboard[elf1] + scoreboard[elf2];
        if sum >= 10 {
            scoreboard.push(sum / 10);
            scoreboard.push(sum % 10);
        } else {
            scoreboard.push(sum);
        }

        elf1 = (elf1 + 1 + scoreboard[elf1]) % scoreboard.len();
        elf2 = (elf2 + 1 + scoreboard[elf2]) % scoreboard.len();
    }

    let result: String = scoreboard[recipes..recipes + 10].iter().map(|&x| x.to_string()).collect();
    println!("{}", result);
}