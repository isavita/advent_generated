
use std::fs;
use std::str::FromStr;

fn main() {
    let content = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = content.trim().split('\n').collect();

    let mut k = Vec::new();
    let mut l = Vec::new();
    let mut m = Vec::new();

    for (i, line) in lines.iter().enumerate() {
        match i % 18 {
            4 => {
                let v = line.split_whitespace().nth(2).unwrap().parse::<i64>().unwrap();
                l.push(v);
            }
            5 => {
                let v = line.split_whitespace().nth(2).unwrap().parse::<i64>().unwrap();
                k.push(v);
            }
            15 => {
                let v = line.split_whitespace().nth(2).unwrap().parse::<i64>().unwrap();
                m.push(v);
            }
            _ => {}
        }
    }

    let mut constraints = std::collections::HashMap::new();
    let mut stack = Vec::new();

    for i in 0..l.len() {
        match l[i] {
            1 => {
                stack.push(i);
            }
            26 => {
                let pop = stack.pop().unwrap();
                constraints.insert(pop, (i, m[pop] + k[i]));
            }
            _ => {}
        }
    }

    let mut min = [0; 14];
    for i in 0..14 {
        if let Some(&(j, sum)) = constraints.get(&i) {
            let mut vmin = 1;
            while vmin + sum < 1 {
                vmin += 1;
            }
            min[i] = vmin;
            min[j] = vmin + sum;
        }
    }

    println!("{}", num(&min));
}

fn num(w: &[i64; 14]) -> i64 {
    let mut n = 0;
    for &digit in w {
        n = n * 10 + digit;
    }
    n
}
