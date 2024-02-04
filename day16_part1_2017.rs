
use std::fs;

fn main() {
    let mut programs: Vec<char> = (b'a'..=b'p').map(|c| c as char).collect();
    let input = fs::read_to_string("input.txt").unwrap();
    let moves: Vec<&str> = input.trim().split(',').collect();

    for m in moves {
        match &m[0..1] {
            "s" => {
                let x: usize = m[1..].parse().unwrap();
                programs.rotate_right(x);
            }
            "x" => {
                let positions: Vec<usize> = m[1..].split('/').map(|s| s.parse().unwrap()).collect();
                programs.swap(positions[0], positions[1]);
            }
            "p" => {
                let partners: Vec<&str> = m[1..].split('/').collect();
                let pos_a = programs.iter().position(|&c| c == partners[0].chars().next().unwrap()).unwrap();
                let pos_b = programs.iter().position(|&c| c == partners[1].chars().next().unwrap()).unwrap();
                programs.swap(pos_a, pos_b);
            }
            _ => {}
        }
    }

    println!("{}", programs.into_iter().collect::<String>());
}
