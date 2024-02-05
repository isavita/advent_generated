
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut password: Vec<char> = "abcdefgh".chars().collect();

    for line in input.lines() {
        let words: Vec<&str> = line.split_whitespace().collect();
        match words[0] {
            "swap" => match words[1] {
                "position" => {
                    let x: usize = words[2].parse().unwrap();
                    let y: usize = words[5].parse().unwrap();
                    password.swap(x, y);
                }
                "letter" => {
                    let x = words[2].chars().next().unwrap();
                    let y = words[5].chars().next().unwrap();
                    let x_pos = password.iter().position(|&c| c == x).unwrap();
                    let y_pos = password.iter().position(|&c| c == y).unwrap();
                    password.swap(x_pos, y_pos);
                }
                _ => {}
            },
            "rotate" => match words[1] {
                "left" => {
                    let steps: usize = words[2].parse().unwrap();
                    for _ in 0..steps {
                        let c = password.remove(0);
                        password.push(c);
                    }
                }
                "right" => {
                    let steps: usize = words[2].parse().unwrap();
                    for _ in 0..steps {
                        let c = password.pop().unwrap();
                        password.insert(0, c);
                    }
                }
                "based" => {
                    let x = words[6].chars().next().unwrap();
                    let x_pos = password.iter().position(|&c| c == x).unwrap();
                    let steps = 1 + x_pos + if x_pos >= 4 { 1 } else { 0 };
                    for _ in 0..steps {
                        let c = password.pop().unwrap();
                        password.insert(0, c);
                    }
                }
                _ => {}
            },
            "reverse" => {
                let x: usize = words[2].parse().unwrap();
                let y: usize = words[4].parse().unwrap();
                password[x..=y].reverse();
            }
            "move" => {
                let x: usize = words[2].parse().unwrap();
                let y: usize = words[5].parse().unwrap();
                let c = password.remove(x);
                password.insert(y, c);
            }
            _ => {}
        }
    }

    println!("{}", password.iter().collect::<String>());
}
