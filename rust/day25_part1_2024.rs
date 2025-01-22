
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines = io::BufReader::new(file).lines();
    let raw: Vec<String> = lines.filter_map(Result::ok).filter(|l| !l.trim().is_empty()).collect();

    if raw.len() % 7 != 0 {
        println!("0");
        return Ok(());
    }

    let mut locks = Vec::new();
    let mut keys = Vec::new();

    for i in (0..raw.len()).step_by(7) {
        let block = &raw[i..i + 7];
        if block.iter().any(|ln| ln.len() < 5) {
          continue;
        }
        if all_char(&block[0], '#') {
          locks.push(parse_lock(block));
        } else {
          keys.push(parse_key(block));
        }
    }

    let count = locks.iter().flat_map(|lock| {
        keys.iter().filter(move |key| fits(lock, key))
    }).count();

    println!("{}", count);
    Ok(())
}

fn parse_lock(b: &[String]) -> [u8; 5] {
    let mut h = [0u8; 5];
    for c in 0..5 {
        for r in 1..7 {
            if b[r].as_bytes()[c] == b'#' {
                h[c] += 1;
            } else {
                break;
            }
        }
    }
    h
}

fn parse_key(b: &[String]) -> [u8; 5] {
    let mut h = [0u8; 5];
    for c in 0..5 {
        for r in (0..6).rev() {
            if b[r].as_bytes()[c] == b'#' {
                h[c] += 1;
            } else {
                break;
            }
        }
    }
    h
}

fn fits(lock: &[u8; 5], key: &[u8; 5]) -> bool {
    lock.iter().zip(key).all(|(l,k)| l + k <= 5)
}

fn all_char(s: &str, ch: char) -> bool {
    s.chars().all(|c| c == ch)
}
