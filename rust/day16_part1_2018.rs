
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Unable to read file");
    let lines: Vec<&str> = input.trim().split('\n').collect();

    let opcodes: Vec<Op> = vec![
        Op { name: "addr", action: '+', a: 'r', b: 'r', matches: Vec::new() },
        Op { name: "addi", action: '+', a: 'r', b: 'v', matches: Vec::new() },
        Op { name: "mulr", action: '*', a: 'r', b: 'r', matches: Vec::new() },
        Op { name: "muli", action: '*', a: 'r', b: 'v', matches: Vec::new() },
        Op { name: "banr", action: '&', a: 'r', b: 'r', matches: Vec::new() },
        Op { name: "bani", action: '&', a: 'r', b: 'v', matches: Vec::new() },
        Op { name: "borr", action: '|', a: 'r', b: 'r', matches: Vec::new() },
        Op { name: "bori", action: '|', a: 'r', b: 'v', matches: Vec::new() },
        Op { name: "setr", action: 'a', a: 'r', b: 'r', matches: Vec::new() },
        Op { name: "seti", action: 'a', a: 'v', b: 'r', matches: Vec::new() },
        Op { name: "gtir", action: '>', a: 'v', b: 'r', matches: Vec::new() },
        Op { name: "gtri", action: '>', a: 'r', b: 'v', matches: Vec::new() },
        Op { name: "gtrr", action: '>', a: 'r', b: 'r', matches: Vec::new() },
        Op { name: "eqir", action: '=', a: 'v', b: 'r', matches: Vec::new() },
        Op { name: "eqri", action: '=', a: 'r', b: 'v', matches: Vec::new() },
        Op { name: "eqrr", action: '=', a: 'r', b: 'r', matches: Vec::new() },
    ];

    let mut sum = 0;
    let mut line_count = 0;

    while line_count < lines.len() {
        if !lines[line_count].is_empty() && lines[line_count].starts_with("B") {
            let before_split: Vec<i32> = lines[line_count].split(|c: char| !c.is_ascii_digit()).filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()).collect();
            let registers = [before_split[0], before_split[1], before_split[2], before_split[3]];
            
            let instruction_split: Vec<u8> = lines[line_count+1].split(|c: char| !c.is_ascii_digit()).filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()).collect();
            let instruction = [instruction_split[0],instruction_split[1],instruction_split[2],instruction_split[3]];

            let after_split: Vec<i32> = lines[line_count+2].split(|c: char| !c.is_ascii_digit()).filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()).collect();
            let after = [after_split[0], after_split[1], after_split[2], after_split[3]];


            let temp_sum = test_code(&registers, &after, &instruction, &opcodes);

            if temp_sum >= 3 {
                sum += 1;
            }

            line_count += 4;
        } else {
            break;
        }
    }

    println!("{}", sum);
}

fn test_code(registers: &[i32; 4], after: &[i32; 4], instruction: &[u8; 4], opcodes: &[Op]) -> i32 {
    let mut sum = 0;
    for op in opcodes {
        if match_registers(after, &run_op(op, registers, instruction)) {
            sum+=1;
        }
    }
    sum
}

fn match_registers(r: &[i32; 4], c: &[i32; 4]) -> bool {
    r == c
}

fn run_op(op: &Op, registers: &[i32; 4], instruction: &[u8; 4]) -> [i32; 4] {
    let mut register_cp = *registers;
    let a;
    let b;

    if op.a == 'r' {
        a = register_cp[instruction[1] as usize];
    } else {
        a = instruction[1] as i32;
    }

     if op.b == 'r' {
        b = register_cp[instruction[2] as usize];
    } else {
        b = instruction[2] as i32;
    }


    match op.action {
        '+' => register_cp[instruction[3] as usize] = a + b,
        '*' => register_cp[instruction[3] as usize] = a * b,
        '&' => register_cp[instruction[3] as usize] = a & b,
        '|' => register_cp[instruction[3] as usize] = a | b,
        'a' => register_cp[instruction[3] as usize] = a,
        '>' => register_cp[instruction[3] as usize] = if a > b { 1 } else { 0 },
        '=' => register_cp[instruction[3] as usize] = if a == b { 1 } else { 0 },
         _ => panic!("Invalid action"),
    }

    register_cp
}

#[derive(Clone)]
struct Op {
    a: char,
    b: char,
    action: char,
    name: &'static str,
    matches: Vec<u8>,
}
