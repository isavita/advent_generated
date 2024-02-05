use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut x = 0;
    let mut y = 0;
    let mut houses = std::collections::HashSet::new();
    houses.insert((x, y));

    for c in input.chars() {
        match c {
            '^' => y += 1,
            'v' => y -= 1,
            '>' => x += 1,
            '<' => x -= 1,
            _ => (),
        }
        houses.insert((x, y));
    }

    println!("{}", houses.len());
}