use std::fs;
use std::io;

struct CrateStack {
    crates: Vec<char>,
}

impl CrateStack {
    fn new() -> Self {
        CrateStack { crates: Vec::new() }
    }

    fn push(&mut self, crate_: char) {
        self.crates.push(crate_);
    }

    fn pop(&mut self) -> Option<char> {
        self.crates.pop()
    }

    fn peek(&self) -> Option<char> {
        self.crates.last().copied()
    }
}

fn main() -> io::Result<()> {
    let input = fs::read_to_string("input.txt")?;

    let mut stacks: Vec<CrateStack> = Vec::new();

    // Parse initial stack configuration
    for line in input.lines().take_while(|line| !line.is_empty()) {
        for (i, crate_) in line.chars().skip(1).step_by(4).enumerate() {
            while stacks.len() <= i {
                stacks.push(CrateStack::new());
            }
            if crate_.is_alphabetic() {
                stacks[i].push(crate_);
            }
        }
    }

    // Reverse each stack to maintain the correct order
    for stack in &mut stacks {
        stack.crates.reverse();
    }

    // Parse and execute move instructions
    for line in input.lines().skip_while(|line| !line.starts_with("move")) {
        let mut parts = line.split_whitespace();
        let count: usize = parts.nth(1).unwrap().parse().unwrap();
        let from: usize = parts.nth(1).unwrap().parse().unwrap();
        let to: usize = parts.nth(1).unwrap().parse().unwrap();

        let mut temp: Vec<char> = Vec::new();
        for _ in 0..count {
            temp.push(stacks[from - 1].pop().unwrap());
        }
        temp.reverse();
        for crate_ in temp {
            stacks[to - 1].push(crate_);
        }
    }

    // Print the top crate of each stack
    for stack in &stacks {
        if let Some(crate_) = stack.peek() {
            print!("{}", crate_);
        }
    }
    println!();

    Ok(())
}