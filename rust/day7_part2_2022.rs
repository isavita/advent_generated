
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut root = vec!["".to_string()];
    let mut dirs: HashMap<String, i64> = HashMap::new();
    let mut files: HashMap<String, i64> = HashMap::new();
    let mut curr: Vec<String> = vec![];

    for line in reader.lines() {
        let line = line?;
        let txt: Vec<&str> = line.split_whitespace().collect();

        if txt[0] == "$" {
            if txt[1] == "cd" {
                if txt[2] == "/" {
                    curr = root.clone();
                } else if txt[2] == ".." {
                    curr.pop();
                } else {
                   curr.push(txt[2].to_string());
                }
                dirs.insert(curr.join("/"), 0);
            }
        } else if txt[0] != "dir"{
              files.insert(format!("{}/{}",curr.join("/"), txt[1]), txt[0].parse::<i64>().unwrap());
        }
    }


    for (f, s) in files.iter() {
        let path: Vec<&str> = f.split("/").collect();
        for i in 1..path.len() {
            let dir_path = path[0..i].join("/");
            *dirs.entry(dir_path).or_insert(0) += s;
        }
    }

    let mut sorted_sizes: Vec<i64> = dirs.values().cloned().collect();
    sorted_sizes.sort_unstable();

    let total: i64 = 70000000;
    let want: i64 = 30000000;
    let available: i64 = total - dirs[""];
    let ans = sorted_sizes.binary_search(&(want - available)).map_or_else(|i| sorted_sizes[i], |i| sorted_sizes[i]);

    println!("{}", ans);
    Ok(())
}
