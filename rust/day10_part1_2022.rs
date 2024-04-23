use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines = io::BufReader::new(file).lines();
    let mut x = vec![1];
    for line in lines {
        let line = line?;
        match line.as_str() {
            "noop" => x.push(x.last().unwrap().clone()),
            _ => {
                let mut parts = line.split_whitespace();
                let _ = parts.next();
                let n = parts.next().unwrap().parse::<i32>().unwrap();
                x.push(x.last().unwrap().clone());
                x.push(x.last().unwrap() + n);
            }
        }
    }

    let mut sum = 0;
    for i in 0..x.len() {
        if (i as i32 - 19) % 40 == 0 {
            sum += (i as i32 + 1) * x[i as usize];
        }
    }
    println!("{}", sum);
    Ok(())
}