use std::fs;

fn main() {
    let contents = fs::read_to_string("input.txt")
        .expect("Something went wrong reading the file");

    let mut registers = std::collections::HashMap::new();

    for line in contents.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let reg = parts[0];
        let op = parts[1];
        let amount: i32 = parts[2].parse().unwrap();
        let condReg = parts[4];
        let condOp = parts[5];
        let condVal: i32 = parts[6].parse().unwrap();

        let mut cond = false;
        match condOp {
            ">" => cond = registers.get(condReg).unwrap_or(&0) > &condVal,
            ">=" => cond = registers.get(condReg).unwrap_or(&0) >= &condVal,
            "<" => cond = registers.get(condReg).unwrap_or(&0) < &condVal,
            "<=" => cond = registers.get(condReg).unwrap_or(&0) <= &condVal,
            "==" => cond = registers.get(condReg).unwrap_or(&0) == &condVal,
            "!=" => cond = registers.get(condReg).unwrap_or(&0) != &condVal,
            _ => {}
        }

        if cond {
            match op {
                "inc" => *registers.entry(reg.to_string()).or_insert(0) += amount,
                "dec" => *registers.entry(reg.to_string()).or_insert(0) -= amount,
                _ => {}
            }
        }
    }

    let max_value = registers.values().cloned().max().unwrap_or(0);
    println!("{}", max_value);
}