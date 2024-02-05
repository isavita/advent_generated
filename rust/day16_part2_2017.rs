
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let moves: Vec<&str> = input.trim().split(',').collect();
    
    let mut programs: Vec<char> = (b'a'..=b'p').map(|x| x as char).collect();
    
    let mut seen = Vec::new();
    
    for _ in 0..1_000_000_000 {
        if seen.contains(&programs) {
            println!("{}", seen[1_000_000_000 % seen.len()].iter().collect::<String>());
            break;
        }
        
        seen.push(programs.clone());
        
        for m in &moves {
            match &m[0..1] {
                "s" => {
                    let n: usize = m[1..].parse().unwrap();
                    programs.rotate_right(n);
                },
                "x" => {
                    let ab: Vec<usize> = m[1..].split('/').map(|x| x.parse().unwrap()).collect();
                    programs.swap(ab[0], ab[1]);
                },
                "p" => {
                    let ab: Vec<char> = m[1..].split('/').map(|x| x.chars().next().unwrap()).collect();
                    let a = programs.iter().position(|&x| x == ab[0]).unwrap();
                    let b = programs.iter().position(|&x| x == ab[1]).unwrap();
                    programs.swap(a, b);
                },
                _ => panic!("Invalid move"),
            }
        }
    }
}
