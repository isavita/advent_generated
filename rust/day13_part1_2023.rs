
use std::fs;

struct Mirror {
    rows: Vec<i32>,
    cols: Vec<i32>,
}

fn parse_input(input: Vec<String>) -> Vec<Mirror> {
    let mut mirrors: Vec<Mirror> = vec![];
    let mut mirror_str: Vec<String> = vec![];

    for line in input {
        if line == "" {
            mirrors.push(parse_mirror(mirror_str.clone()));
            mirror_str = vec![];
        } else {
            mirror_str.push(line);
        }
    }
    mirrors.push(parse_mirror(mirror_str));

    mirrors
}

fn parse_mirror(mirror_str: Vec<String>) -> Mirror {
    let mut mirror = Mirror {
        rows: vec![0; mirror_str.len()],
        cols: vec![0; mirror_str[0].len()],
    };

    for (y, line) in mirror_str.iter().enumerate() {
        for (x, char) in line.chars().enumerate() {
            mirror.rows[y] <<= 1;
            mirror.cols[x] <<= 1;
            if char == '#' {
                mirror.rows[y] += 1;
                mirror.cols[x] += 1;
            }
        }
    }

    mirror
}

fn get_mirror_axis(lines: &Vec<i32>) -> i32 {
    for i in 1..lines.len() {
        let mut is_mirror = true;

        for j in 0..std::cmp::min(i, lines.len() - i) {
            if lines[i - 1 - j] != lines[i + j] {
                is_mirror = false;
            }
        }

        if is_mirror {
            return i as i32;
        }
    }

    0
}

fn solve(input: Vec<String>) -> i32 {
    let mirrors = parse_input(input);
    let mut res = 0;

    for mirror in mirrors {
        res += get_mirror_axis(&mirror.cols);
        res += get_mirror_axis(&mirror.rows) * 100;
    }

    res
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let input_lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();
    println!("{}", solve(input_lines));
}
