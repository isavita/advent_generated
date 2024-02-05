
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn get_value(arg: &str, registers: &HashMap<String, isize>) -> isize {
    arg.parse::<isize>().unwrap_or_else(|_| *registers.get(arg).unwrap_or(&0))
}

fn main() -> io::Result<()> {
    let path = Path::new("input.txt");
    let file = File::open(&path)?;
    let mut instructions = Vec::new();

    for line in io::BufReader::new(file).lines() {
        let instruction: Vec<String> = line?.split_whitespace().map(String::from).collect();
        instructions.push(instruction);
    }

    let mut registers0 = HashMap::new();
    registers0.insert("p".to_string(), 0);
    let mut registers1 = HashMap::new();
    registers1.insert("p".to_string(), 1);
    let mut queue0 = Vec::new();
    let mut queue1 = Vec::new();
    let mut send_count1 = 0;
    let (mut i0, mut i1) = (0, 0);
    let (mut deadlock0, mut deadlock1) = (false, false);

    while !(deadlock0 && deadlock1) {
        deadlock0 = true;
        deadlock1 = true;

        // Program 0
        while i0 < instructions.len() {
            let instruction = &instructions[i0];
            let cmd = &instruction[0];
            let arg1 = &instruction[1];
            match cmd.as_str() {
                "snd" => queue1.push(get_value(arg1, &registers0)),
                "set" => {
                    registers0.insert(arg1.clone(), get_value(&instruction[2], &registers0));
                }
                "add" => {
                    *registers0.entry(arg1.clone()).or_insert(0) += get_value(&instruction[2], &registers0);
                }
                "mul" => {
                    *registers0.entry(arg1.clone()).or_insert(0) *= get_value(&instruction[2], &registers0);
                }
                "mod" => {
                    *registers0.entry(arg1.clone()).or_insert(0) %= get_value(&instruction[2], &registers0);
                }
                "rcv" => {
                    if queue0.is_empty() {
                        break;
                    }
                    registers0.insert(arg1.clone(), queue0.remove(0));
                }
                "jgz" => {
                    if get_value(arg1, &registers0) > 0 {
                        i0 = (i0 as isize + get_value(&instruction[2], &registers0)) as usize;
                        continue;
                    }
                }
                _ => {}
            }
            i0 += 1;
            deadlock0 = false;
        }

        // Program 1
        while i1 < instructions.len() {
            let instruction = &instructions[i1];
            let cmd = &instruction[0];
            let arg1 = &instruction[1];
            match cmd.as_str() {
                "snd" => {
                    queue0.push(get_value(arg1, &registers1));
                    send_count1 += 1;
                }
                "set" => {
                    registers1.insert(arg1.clone(), get_value(&instruction[2], &registers1));
                }
                "add" => {
                    *registers1.entry(arg1.clone()).or_insert(0) += get_value(&instruction[2], &registers1);
                }
                "mul" => {
                    *registers1.entry(arg1.clone()).or_insert(0) *= get_value(&instruction[2], &registers1);
                }
                "mod" => {
                    *registers1.entry(arg1.clone()).or_insert(0) %= get_value(&instruction[2], &registers1);
                }
                "rcv" => {
                    if queue1.is_empty() {
                        break;
                    }
                    registers1.insert(arg1.clone(), queue1.remove(0));
                }
                "jgz" => {
                    if get_value(arg1, &registers1) > 0 {
                        i1 = (i1 as isize + get_value(&instruction[2], &registers1)) as usize;
                        continue;
                    }
                }
                _ => {}
            }
            i1 += 1;
            deadlock1 = false;
        }
    }

    println!("{}", send_count1);
    Ok(())
}
