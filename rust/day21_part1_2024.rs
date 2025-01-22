
use std::fs;
use std::cmp;

#[derive(Clone, Copy)]
struct Position {
    i: i32,
    j: i32,
}

fn find_position(mat: &Vec<String>, ch: u8) -> Position {
    for (i, row) in mat.iter().enumerate() {
        for (j, &byte) in row.as_bytes().iter().enumerate() {
            if byte == ch {
                return Position {
                    i: i as i32,
                    j: j as i32,
                };
            }
        }
    }
    Position { i: -1, j: -1 }
}

fn is_valid_move(mat: &Vec<String>, pos: Position) -> bool {
    pos.i >= 0 && pos.i < mat.len() as i32 && pos.j >= 0 && pos.j < mat[0].len() as i32
}

fn ok(mat: &Vec<String>, mut curr: Position, seq: &str) -> bool {
    for &ch in seq.as_bytes() {
        if !is_valid_move(mat, curr) {
            return false;
        }
        if mat[curr.i as usize].as_bytes()[curr.j as usize] == b' ' {
            return false;
        }

        match ch {
            b'^' => curr.i -= 1,
            b'v' => curr.i += 1,
            b'<' => curr.j -= 1,
            b'>' => curr.j += 1,
            _ => {},
        }
        
    }
    is_valid_move(mat, curr)
}


fn generate_moves(position: Position, objective: u8, pad: &Vec<String>) -> String {
    let obj_pos = find_position(pad, objective);
    
    let mut ret = String::new();
    let mut temp_ret = String::new();

    if position.j > obj_pos.j {
      temp_ret.push_str(&"<".repeat((position.j - obj_pos.j) as usize));
    }
    if position.i > obj_pos.i {
        temp_ret.push_str(&"^".repeat((position.i - obj_pos.i) as usize));
    }
    if position.i < obj_pos.i {
      temp_ret.push_str(&"v".repeat((obj_pos.i - position.i) as usize));
    }
    if position.j < obj_pos.j {
      temp_ret.push_str(&">".repeat((obj_pos.j - position.j) as usize));
    }

    if ok(pad, position, &temp_ret) {
        ret.push_str(&temp_ret);
    } else {
        temp_ret.clear();
        if position.j < obj_pos.j {
          temp_ret.push_str(&">".repeat((obj_pos.j - position.j) as usize));
        }
        if position.i > obj_pos.i {
          temp_ret.push_str(&"^".repeat((position.i - obj_pos.i) as usize));
        }
        if position.i < obj_pos.i {
          temp_ret.push_str(&"v".repeat((obj_pos.i - position.i) as usize));
        }
        if position.j > obj_pos.j {
          temp_ret.push_str(&"<".repeat((position.j - obj_pos.j) as usize));
        }
      
      ret.push_str(&temp_ret)
    }
    
    ret
}


fn solve(code: &str, robots: i32, key_pad: &Vec<String>, robot_pad: &Vec<String>, max_robots: i32) -> i32 {
    if robots <= 0 {
        return code.len() as i32;
    }

    let mut ret = 0;
    let mut posi = 3;
    let mut posj = 2;
    if robots != max_robots {
        posi = 0;
    }
    let mut moves: String;

    for &ch in code.as_bytes() {
        if robots == max_robots {
            moves = generate_moves(Position {i:posi, j:posj}, ch, key_pad);
            let pos = find_position(key_pad, ch);
             posi = pos.i;
             posj = pos.j;
        } else {
            moves = generate_moves(Position{i:posi, j:posj}, ch, robot_pad);
             let pos = find_position(robot_pad, ch);
             posi = pos.i;
             posj = pos.j;
        }
        ret += solve(&(moves + "A"), robots - 1, key_pad, robot_pad, max_robots);
    }
    ret
}

fn main() {
    let content = fs::read_to_string("input.txt").expect("Unable to read file");

    let max_robots = 3;
    let key_pad = vec![
        "789".to_string(),
        "456".to_string(),
        "123".to_string(),
        " 0A".to_string(),
    ];
    let robot_pad = vec![
        " ^A".to_string(),
        "<v>".to_string(),
    ];

    let mut ret = 0;
    for line in content.lines() {
        let trimmed_line = line.trim();
        if trimmed_line.is_empty() {
            continue;
        }

        let mut numeric_part = 0;
        for &byte in trimmed_line.as_bytes() {
            if byte >= b'0' && byte <= b'9' {
                numeric_part = numeric_part * 10 + (byte - b'0') as i32;
            }
        }

        ret += solve(trimmed_line, max_robots, &key_pad, &robot_pad, max_robots) * numeric_part;
    }

    println!("{}", ret);
}
