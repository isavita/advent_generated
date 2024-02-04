use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let mut registers = std::collections::HashMap::new();
    let mut max_value = 0;

    for line in input.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let reg = parts[0];
        let op = parts[1];
        let val: i32 = parts[2].parse().unwrap();
        let cond_reg = parts[4];
        let cond_op = parts[5];
        let cond_val: i32 = parts[6].parse().unwrap();

        let cond_reg_val = *registers.get(cond_reg).unwrap_or(&0);
        let cond_check = match cond_op {
            ">" => cond_reg_val > cond_val,
            "<" => cond_reg_val < cond_val,
            ">=" => cond_reg_val >= cond_val,
            "<=" => cond_reg_val <= cond_val,
            "==" => cond_reg_val == cond_val,
            "!=" => cond_reg_val != cond_val,
            _ => false,
        };

        if cond_check {
            let reg_val = registers.entry(reg).or_insert(0);
            match op {
                "inc" => *reg_val += val,
                "dec" => *reg_val -= val,
                _ => (),
            }
            if *reg_val > max_value {
                max_value = *reg_val;
            }
        }
    }

    println!("{}", registers.values().max().unwrap_or(&0));
    println!("{}", max_value);
}