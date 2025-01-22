
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::PathBuf;

fn main() -> io::Result<()> {
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut sizes: HashMap<PathBuf, usize> = HashMap::new();
    let mut current_path: PathBuf = PathBuf::new();

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();

        match parts[0] {
            "$" => {
                if parts[1] == "cd" {
                    match parts[2] {
                        ".." => {
                            current_path.pop();
                        }
                        "/" => {
                            current_path = PathBuf::from("/");
                        }
                        dir => {
                           current_path.push(dir);
                        }
                    }
                }
            }
            "dir" => {
                // Ignore directories
            }
            size => {
               
                if let Ok(file_size) = size.parse::<usize>() {
                     let mut path_copy = current_path.clone();
                      
                    loop {
                        *sizes.entry(path_copy.clone()).or_insert(0) += file_size;
                        if !path_copy.pop() {
                            break;
                        }
                    }
                }
            }
        }
    }

    let total_size: usize = sizes
        .values()
        .filter(|&size| *size <= 100000)
        .sum();

    println!("{}", total_size);

    Ok(())
}
