
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let program: Vec<i64> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();

    let mut memory = vec![0; 10000];
    for (i, &value) in program.iter().enumerate() {
        memory[i] = value;
    }

    let mut ip: usize = 0;
    let mut relative_base: i64 = 0;

    loop {
        let opcode = memory[ip] % 100;
        if opcode == 99 {
            break;
        }

        let get_param = |offset| {
            let mode = (memory[ip] / 10i64.pow((offset + 1) as u32)) % 10;
            match mode {
                0 => memory[memory[ip + offset] as usize],
                1 => memory[ip + offset],
                2 => memory[(memory[ip + offset] + relative_base) as usize],
                _ => unreachable!(),
            }
        };

        let get_dest = |offset| {
            let mode = (memory[ip] / 10i64.pow((offset + 1) as u32)) % 10;
            match mode {
                0 => memory[ip + offset] as usize,
                2 => (memory[ip + offset] + relative_base) as usize,
                _ => unreachable!(),
            }
        };

        match opcode {
            1 => {
                let dest = get_dest(3);
                memory[dest] = get_param(1) + get_param(2);
                ip += 4;
            }
            2 => {
                let dest = get_dest(3);
                memory[dest] = get_param(1) * get_param(2);
                ip += 4;
            }
            3 => {
                let dest = get_dest(1);
                memory[dest] = 1; // Input value
                ip += 2;
            }
            4 => {
                println!("{}", get_param(1));
                ip += 2;
            }
            5 => {
                if get_param(1) != 0 {
                    ip = get_param(2) as usize;
                } else {
                    ip += 3;
                }
            }
            6 => {
                if get_param(1) == 0 {
                    ip = get_param(2) as usize;
                } else {
                    ip += 3;
                }
            }
            7 => {
                let dest = get_dest(3);
                memory[dest] = if get_param(1) < get_param(2) { 1 } else { 0 };
                ip += 4;
            }
            8 => {
                let dest = get_dest(3);
                memory[dest] = if get_param(1) == get_param(2) { 1 } else { 0 };
                ip += 4;
            }
            9 => {
                relative_base += get_param(1);
                ip += 2;
            }
            _ => unreachable!(),
        }
    }
}
