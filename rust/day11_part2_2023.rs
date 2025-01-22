
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

fn solve(filename: &str, expansion_factor: usize) -> usize {
    let mut universe: Vec<Vec<char>> = Vec::new();
    if let Ok(lines) = read_lines(filename) {
        for line in lines {
            if let Ok(line) = line {
                universe.push(line.chars().collect());
            }
        }
    }

    let rows = universe.len();
    let cols = universe[0].len();

    let mut empty_rows: Vec<usize> = Vec::new();
    for r in 0..rows {
        if universe[r].iter().all(|&c| c == '.') {
            empty_rows.push(r);
        }
    }

    let mut empty_cols: Vec<usize> = Vec::new();
    for c in 0..cols {
        let mut is_empty = true;
        for r in 0..rows {
            if universe[r][c] == '#' {
                is_empty = false;
                break;
            }
        }
        if is_empty {
            empty_cols.push(c);
        }
    }

    let mut galaxies: Vec<(usize, usize)> = Vec::new();
    for r in 0..rows {
        for c in 0..cols {
            if universe[r][c] == '#' {
                galaxies.push((r, c));
            }
        }
    }

    let mut total_distance = 0;
    for i in 0..galaxies.len() {
        for j in i + 1..galaxies.len() {
            let (r1, c1) = galaxies[i];
            let (r2, c2) = galaxies[j];

            let mut expanded_r1 = r1;
            for &er in &empty_rows {
                if er < r1 {
                    expanded_r1 += expansion_factor - 1;
                }
            }
            let mut expanded_c1 = c1;
            for &ec in &empty_cols {
                if ec < c1 {
                    expanded_c1 += expansion_factor - 1;
                }
            }
            
            let mut expanded_r2 = r2;
            for &er in &empty_rows {
                if er < r2 {
                    expanded_r2 += expansion_factor - 1;
                }
            }
            let mut expanded_c2 = c2;
            for &ec in &empty_cols {
                if ec < c2 {
                    expanded_c2 += expansion_factor - 1;
                }
            }


            let distance = expanded_r1.abs_diff(expanded_r2) + expanded_c1.abs_diff(expanded_c2);
            total_distance += distance;
        }
    }

    total_distance
}


fn main() {
    let filename = "input.txt";

    let part1_answer = solve(filename, 2);
    println!("Part 1: {}", part1_answer);

    let part2_answer = solve(filename, 1000000);
    println!("Part 2: {}", part2_answer);
}
