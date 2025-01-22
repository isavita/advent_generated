
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::str::FromStr;

#[derive(Debug)]
struct Move {
    count: usize,
    from: usize,
    to: usize,
}

impl FromStr for Move {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split_whitespace().collect();
        if parts.len() != 6 || parts[0] != "move" || parts[2] != "from" || parts[4] != "to" {
            return Err(());
        }

        let count = parts[1].parse().map_err(|_| ())?;
        let from = parts[3].parse().map_err(|_| ())?;
        let to = parts[5].parse().map_err(|_| ())?;

        Ok(Move { count, from, to })
    }
}

fn main() {
    let file = File::open("input.txt").expect("Unable to open input.txt");
    let reader = BufReader::new(file);

    let mut lines = reader.lines().map(|l| l.expect("Failed to read line"));

    // Parse initial state
    let mut initial_state_lines = Vec::new();
    while let Some(line) = lines.next() {
        if line.is_empty() {
            break;
        }
        initial_state_lines.push(line);
    }

    let num_stacks = (initial_state_lines[0].len() + 1) / 4;
    let mut stacks: Vec<Vec<char>> = vec![Vec::new(); num_stacks];

    for line in initial_state_lines.iter().rev().skip(1) {
        for (i, chunk) in line.as_bytes().chunks(4).enumerate() {
            if chunk[1] != b' ' {
                stacks[i].push(chunk[1] as char);
            }
        }
    }

    // Parse and apply moves
    for line in lines {
        let move_op: Move = line.parse().expect("Invalid move format");
        for _ in 0..move_op.count {
            if let Some(c) = stacks[move_op.from - 1].pop() {
                stacks[move_op.to - 1].push(c);
            }
        }
    }
    

    // Collect top crates
    let result: String = stacks.iter().filter_map(|stack| stack.last()).collect();
    println!("{}", result);
}
