
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn find_position(mat: &[&str], ch: char) -> (i32, i32) {
    for (i, row) in mat.iter().enumerate() {
        for (j, c) in row.chars().enumerate() {
            if c == ch {
                return (i as i32, j as i32);
            }
        }
    }
    (-1, -1)
}

fn ok(mat: &[&str], st: (i32, i32), seq: &str) -> bool {
    let (mut curr_i, mut curr_j) = st;
    for ch in seq.chars() {
        if !(0 <= curr_i && curr_i < mat.len() as i32 && 0 <= curr_j && curr_j < mat[curr_i as usize].len() as i32)
            || mat[curr_i as usize].chars().nth(curr_j as usize).unwrap() == ' '
        {
            return false;
        }
        match ch {
            '^' => curr_i -= 1,
            'v' => curr_i += 1,
            '<' => curr_j -= 1,
            '>' => curr_j += 1,
            _ => {}
        }
    }
    true
}

fn generate_moves(position: (i32, i32), objective: char, pad: &[&str]) -> String {
    let (obj_pos_i, obj_pos_j) = find_position(pad, objective);
    let (pos_i, pos_j) = position;
    let mut result = String::new();

    if pos_j > obj_pos_j {
        result.push_str(&"<".repeat((pos_j - obj_pos_j) as usize));
    }
    if pos_i > obj_pos_i {
        result.push_str(&"^".repeat((pos_i - obj_pos_i) as usize));
    }
    if pos_i < obj_pos_i {
        result.push_str(&"v".repeat((obj_pos_i - pos_i) as usize));
    }
    if pos_j < obj_pos_j {
        result.push_str(&">".repeat((obj_pos_j - pos_j) as usize));
    }

    if !ok(pad, position, &result) {
        result.clear();
        if pos_j < obj_pos_j {
            result.push_str(&">".repeat((obj_pos_j - pos_j) as usize));
        }
        if pos_i > obj_pos_i {
            result.push_str(&"^".repeat((pos_i - obj_pos_i) as usize));
        }
        if pos_i < obj_pos_i {
            result.push_str(&"v".repeat((obj_pos_i - pos_i) as usize));
        }
        if pos_j > obj_pos_j {
            result.push_str(&"<".repeat((pos_j - obj_pos_j) as usize));
        }
    }
    result
}

fn solve(
    code: &str,
    robots: i32,
    key_pad: &[&str],
    robot_pad: &[&str],
    max_robots: i32,
    memo: &mut HashMap<(String, i32, i32), usize>,
) -> usize {
    let key = (code.to_string(), robots, max_robots);
    if let Some(&ret) = memo.get(&key) {
        return ret;
    }

    if robots <= 0 {
        return code.len();
    }

    let mut ret = 0;
    let (mut pos_i, mut pos_j) = (3, 2);
    if robots != max_robots {
        pos_i = 0;
    }

    for ch in code.chars() {
        let moves = if robots == max_robots {
            let m = generate_moves((pos_i, pos_j), ch, key_pad);
            pos_i = find_position(key_pad, ch).0;
            pos_j = find_position(key_pad, ch).1;
            m
        } else {
            let m = generate_moves((pos_i, pos_j), ch, robot_pad);
            pos_i = find_position(robot_pad, ch).0;
            pos_j = find_position(robot_pad, ch).1;
            m
        };
        ret += solve(&(moves + "A"), robots - 1, key_pad, robot_pad, max_robots, memo);
    }
    memo.insert(key, ret);
    ret
}

fn main() {
    let file = File::open("input.txt").expect("Could not open file");
    let reader = BufReader::new(file);

    let max_robots = 26;
    let key_pad = ["789", "456", "123", " 0A"];
    let robot_pad = [" ^A", "<v>"];

    let mut ret = 0;
    let mut memo = HashMap::new();

    for line in reader.lines() {
        let code = line.expect("Could not read line").trim().to_string();
        if code.is_empty() {
            continue;
        }

        let mut numeric_part = 0;
        for char in code.chars() {
            if char.is_digit(10) {
                numeric_part = numeric_part * 10 + char.to_digit(10).unwrap() as usize;
            }
        }

        ret += solve(&code, max_robots, &key_pad, &robot_pad, max_robots, &mut memo) * numeric_part;
    }
    println!("{}", ret);
}
