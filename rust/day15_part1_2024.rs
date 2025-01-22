
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut grid = Vec::new();
    let mut moves = String::new();
    let mut reading_map = true;

    for line in reader.lines() {
        let line = line?;
        if reading_map {
            if line.contains('#') {
                grid.push(line);
            } else {
                reading_map = false;
                moves.push_str(&line);
            }
        } else {
            moves.push_str(&line);
        }
    }
    let mut runes: Vec<Vec<char>> = grid.iter().map(|row| row.chars().collect()).collect();

    let (mut robot_r, mut robot_c) = (0, 0);
    for r in 0..runes.len() {
        for c in 0..runes[r].len() {
            if runes[r][c] == '@' {
                robot_r = r;
                robot_c = c;
            }
        }
    }

    let dirs: std::collections::HashMap<char, (isize, isize)> =
        [('^', (-1, 0)), ('v', (1, 0)), ('<', (0, -1)), ('>', (0, 1))]
            .iter()
            .cloned()
            .collect();

    for move_char in moves.chars() {
        if let Some(&d) = dirs.get(&move_char) {
            let nr = (robot_r as isize + d.0) as usize;
            let nc = (robot_c as isize + d.1) as usize;

            if runes[nr][nc] == '#' {
                continue;
            } else if runes[nr][nc] == 'O' {
                if !push_boxes(&mut runes, nr, nc, d.0, d.1) {
                    continue;
                }
            }
            if runes[nr][nc] == '.' || runes[nr][nc] == 'O' {
                runes[robot_r][robot_c] = '.';
                runes[nr][nc] = '@';
                robot_r = nr;
                robot_c = nc;
            }
        }
    }

    let mut sum = 0;
    for r in 0..runes.len() {
        for c in 0..runes[r].len() {
            if runes[r][c] == 'O' {
                sum += r * 100 + c;
            }
        }
    }

    println!("{}", sum);
    Ok(())
}

fn push_boxes(runes: &mut Vec<Vec<char>>, r: usize, c: usize, dr: isize, dc: isize) -> bool {
    let nr = (r as isize + dr) as usize;
    let nc = (c as isize + dc) as usize;

    if runes[nr][nc] == '#' {
        return false;
    }
    if runes[nr][nc] == 'O' {
        if !push_boxes(runes, nr, nc, dr, dc) {
            return false;
        }
    }

    if runes[nr][nc] == '.' {
        runes[nr][nc] = 'O';
        runes[r][c] = '.';
        return true;
    }
    false
}
