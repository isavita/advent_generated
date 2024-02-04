use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut x = 0;
    let mut y = 0;
    let mut santa_x = 0;
    let mut santa_y = 0;
    let mut robo_x = 0;
    let mut robo_y = 0;
    let mut visited = std::collections::HashSet::new();
    
    visited.insert((x, y));

    for (i, c) in input.chars().enumerate() {
        if i % 2 == 0 {
            match c {
                '^' => y += 1,
                'v' => y -= 1,
                '>' => x += 1,
                '<' => x -= 1,
                _ => (),
            }
            visited.insert((x, y));
            santa_x = x;
            santa_y = y;
        } else {
            match c {
                '^' => robo_y += 1,
                'v' => robo_y -= 1,
                '>' => robo_x += 1,
                '<' => robo_x -= 1,
                _ => (),
            }
            visited.insert((robo_x, robo_y));
        }
    }

    println!("{}", visited.len());
}