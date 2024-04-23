use std::fs::read_to_string;
use std::collections::HashSet;

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let mut x = vec![1];
    for line in input.trim().split("\n") {
        match line {
            "noop" => x.push(x.last().unwrap().clone()),
            _ => {
                let (_, n) = line.split_once(" ").unwrap();
                let n: i32 = n.parse().unwrap();
                x.push(x.last().unwrap().clone());
                x.push(x.last().unwrap() + n);
            }
        }
    }

    let mut grid: HashSet<(i32, i32)> = HashSet::new();
    for (i, &v) in x.iter().enumerate() {
        let x = i as i32 % 40;
        let y = i as i32 / 40;
        if (x - v).abs() <= 1 {
            grid.insert((x, y));
        } else {
            grid.remove(&(x, y));
        }
    }

    for y in 0..6 {
        for x in 0..40 {
            if grid.contains(&(x, y)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }
}