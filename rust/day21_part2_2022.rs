
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
struct Monkey<'a> {
    name: &'a str,
    val: i64,
    has_val: bool,
    left: Option<&'a str>,
    right: Option<&'a str>,
    op: &'a str,
}

fn solve<'a>(
    monkey: &'a Monkey,
    index: &HashMap<&'a str, Monkey<'a>>,
) -> Option<i64> {
    if monkey.has_val {
        return Some(monkey.val);
    }

    if let (Some(left_name), Some(right_name)) = (monkey.left, monkey.right) {
        if let (Some(left), Some(right)) = (index.get(left_name), index.get(right_name)) {
           if let (Some(l_val), Some(r_val)) = (solve(left, index), solve(right, index)){
            match monkey.op {
                "+" => return Some(l_val + r_val),
                "-" => return Some(l_val - r_val),
                "*" => return Some(l_val * r_val),
                "/" => return Some(l_val / r_val),
                "==" => return Some(if l_val == r_val { 0 } else { 1 }),
                _ => return None
            }
           }
        }
    }
    None
}


fn expect<'a>(
    monkey: &'a Monkey,
    x: i64,
    index: &HashMap<&'a str, Monkey<'a>>,
) -> i64 {
    if monkey.name == "humn" {
        return x;
    }

    let left_val = monkey.left.and_then(|name| index.get(name)).and_then(|m| solve(m, index));
    let right_val = monkey.right.and_then(|name| index.get(name)).and_then(|m| solve(m, index));

    match (left_val, right_val) {
         (None, Some(right)) => {
              if let Some(left_name) = monkey.left{
                let left = index.get(left_name).unwrap();
                match monkey.op{
                    "+" => return expect(left, x - right, index),
                    "-" => return expect(left, x + right, index),
                    "*" => return expect(left, x / right, index),
                    "/" => return expect(left, x * right, index),
                    "==" => return expect(left, right, index),
                    _ => panic!("invalid op")
                }
             }else {
                panic!("invalid left name")
             }

          }
          (Some(left), None) => {
            if let Some(right_name) = monkey.right{
                let right = index.get(right_name).unwrap();
                match monkey.op {
                    "+" => return expect(right, x - left, index),
                    "-" => return expect(right, left - x, index),
                    "*" => return expect(right, x / left, index),
                    "/" => return expect(right, left / x, index),
                    "==" => return expect(right, left, index),
                    _ => panic!("invalid op")
                }
            }else{
                panic!("invalid right name")
            }
        },
        _ => panic!("impossible")

    }
}

fn parse<'a>(lines: impl Iterator<Item = &'a String>) -> HashMap<&'a str, Monkey<'a>> {
    let mut index = HashMap::new();

    for line in lines {
        let parts: Vec<&str> = line.split(": ").collect();
        let goal = parts[0];
        let value_part = parts[1];

        if let Ok(val) = value_part.parse::<i64>() {
            index.insert(goal, Monkey {
                name: goal,
                val,
                has_val: true,
                left: None,
                right: None,
                op: "",
            });
        } else {
            let r: Vec<&str> = value_part.split(" ").collect();
            let left = r[0];
            let op = r[1];
            let right = r[2];

            index.insert(goal, Monkey {
                name: goal,
                val: 0,
                has_val: false,
                left: Some(left),
                right: Some(right),
                op,
            });
        }
    }

    index
}

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let lines: Vec<String> = io::BufReader::new(file)
        .lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let mut index = parse(lines.iter());

    if let Some(root) = index.get_mut("root"){
         root.op = "==";
    }

    if let Some(humn) = index.get_mut("humn"){
        humn.has_val = false;
    }


    let root = index.get("root").unwrap();

    println!("{}", expect(root, 0, &index));

    Ok(())
}
