use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut tiles = std::collections::HashSet::new();
    for line in input.lines() {
        let mut x = 0;
        let mut y = 0;
        let mut chars = line.chars().collect::<Vec<char>>();
        while !chars.is_empty() {
            match chars.remove(0) {
                'e' => x += 1,
                'w' => x -= 1,
                'n' => {
                    y += 1;
                    match chars.remove(0) {
                        'e' => x += 1,
                        'w' => {}
                        _ => unreachable!(),
                    }
                }
                's' => {
                    y -= 1;
                    match chars.remove(0) {
                        'e' => {}
                        'w' => x -= 1,
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }
        }
        if !tiles.remove(&(x, y)) {
            tiles.insert((x, y));
        }
    }
    println!("{}", tiles.len());

    for _ in 0..100 {
        let mut new_tiles = std::collections::HashSet::new();
        let mut counts = std::collections::HashMap::new();
        for &(x, y) in &tiles {
            for (dx, dy) in &[(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1)] {
                *counts.entry((x + dx, y + dy)).or_insert(0) += 1;
            }
        }
        for (&(x, y), &count) in &counts {
            if count == 2 || (count == 1 && tiles.contains(&(x, y))) {
                new_tiles.insert((x, y));
            }
        }
        tiles = new_tiles;
    }
    println!("{}", tiles.len());
}