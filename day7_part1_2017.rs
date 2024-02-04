
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut programs = vec![];
    let mut above_programs = vec![];

    for line in input.lines() {
        let parts: Vec<&str> = line.split(" -> ").collect();
        let program: Vec<&str> = parts[0].split_whitespace().collect();
        programs.push(program[0]);

        if parts.len() > 1 {
            let above: Vec<&str> = parts[1].split(", ").collect();
            above_programs.extend(above);
        }
    }

    for program in programs {
        if !above_programs.contains(&program) {
            println!("{}", program);
            break;
        }
    }
}
