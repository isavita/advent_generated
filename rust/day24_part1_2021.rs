
use std::fs;

fn main() {
    let content = fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = content.trim().split('\n').collect();

    let mut k = Vec::new();
    let mut l = Vec::new();
    let mut m = Vec::new();

    for (i, line) in lines.iter().enumerate() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        match i % 18 {
            4 => l.push(parts[2].parse::<i32>().unwrap()),
            5 => k.push(parts[2].parse::<i32>().unwrap()),
            15 => m.push(parts[2].parse::<i32>().unwrap()),
            _ => (),
        }
    }

    let mut constraints: Vec<([usize; 2], i32)> = Vec::new();
    let mut stack = Vec::new();

    for i in 0..l.len() {
        match l[i] {
            1 => stack.push(i),
            26 => {
                let pop = stack.pop().unwrap();
                constraints.push(([pop, i], m[pop] + k[i]));
            }
            _ => (),
        }
    }

    let mut max = [0; 14];

    for ([i,j],val) in constraints.iter(){
      let mut vmax=9;
      while vmax + val > 9 {
        vmax -=1;
      }
        max[*i] = vmax;
        max[*j] = vmax + val;
    }

    println!("{}", num(max));
}

fn num(w: [i32; 14]) -> i64 {
    let mut n = 0;
    for v in w {
        n *= 10;
        n += v as i64;
    }
    n
}
