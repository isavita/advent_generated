use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut valid_count = 0;

    for line in input.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let range: Vec<usize> = parts[0]
            .split("-")
            .map(|x| x.parse().unwrap())
            .collect();
        let letter = parts[1].chars().next().unwrap();
        let password = parts[2];

        let pos1 = password.chars().nth(range[0] - 1).unwrap();
        let pos2 = password.chars().nth(range[1] - 1).unwrap();

        if (pos1 == letter || pos2 == letter) && pos1 != pos2 {
            valid_count += 1;
        }
    }

    println!("{}", valid_count);
}