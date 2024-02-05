
use std::fs;

fn run_amplifiers(program: &Vec<i32>, phase_settings: &Vec<i32>) -> i32 {
    let mut input = 0;
    for &phase_setting in phase_settings {
        let mut intcode = program.clone();
        let mut i = 0;
        let mut phase_used = false;
        let mut output = 0;

        loop {
            let opcode = intcode[i] % 100;
            let mode1 = (intcode[i] / 100) % 10;
            let mode2 = (intcode[i] / 1000) % 10;

            match opcode {
                1 | 2 => {
                    let param1 = if mode1 == 1 { intcode[i + 1] } else { intcode[intcode[i + 1] as usize] };
                    let param2 = if mode2 == 1 { intcode[i + 2] } else { intcode[intcode[i + 2] as usize] };
                    let dest = intcode[i + 3] as usize;

                    intcode[dest] = if opcode == 1 { param1 + param2 } else { param1 * param2 };
                    i += 4;
                },
                3 => {
                    let dest = intcode[i + 1] as usize;
                    if !phase_used {
                        intcode[dest] = phase_setting;
                        phase_used = true;
                    } else {
                        intcode[dest] = input;
                    }
                    i += 2;
                },
                4 => {
                    output = if mode1 == 1 { intcode[i + 1] } else { intcode[intcode[i + 1] as usize] };
                    i += 2;
                },
                5 => {
                    let param1 = if mode1 == 1 { intcode[i + 1] } else { intcode[intcode[i + 1] as usize] };
                    let param2 = if mode2 == 1 { intcode[i + 2] } else { intcode[intcode[i + 2] as usize] };

                    if param1 != 0 {
                        i = param2 as usize;
                    } else {
                        i += 3;
                    }
                },
                6 => {
                    let param1 = if mode1 == 1 { intcode[i + 1] } else { intcode[intcode[i + 1] as usize] };
                    let param2 = if mode2 == 1 { intcode[i + 2] } else { intcode[intcode[i + 2] as usize] };

                    if param1 == 0 {
                        i = param2 as usize;
                    } else {
                        i += 3;
                    }
                },
                7 => {
                    let param1 = if mode1 == 1 { intcode[i + 1] } else { intcode[intcode[i + 1] as usize] };
                    let param2 = if mode2 == 1 { intcode[i + 2] } else { intcode[intcode[i + 2] as usize] };
                    let dest = intcode[i + 3] as usize;

                    intcode[dest] = if param1 < param2 { 1 } else { 0 };
                    i += 4;
                },
                8 => {
                    let param1 = if mode1 == 1 { intcode[i + 1] } else { intcode[intcode[i + 1] as usize] };
                    let param2 = if mode2 == 1 { intcode[i + 2] } else { intcode[intcode[i + 2] as usize] };
                    let dest = intcode[i + 3] as usize;

                    intcode[dest] = if param1 == param2 { 1 } else { 0 };
                    i += 4;
                },
                99 => break,
                _ => panic!("Invalid opcode"),
            }
        }

        input = output;
    }

    input
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let program: Vec<i32> = input.trim().split(",").map(|x| x.parse().unwrap()).collect();

    let mut max_output = 0;

    for a in 0..5 {
        for b in 0..5 {
            if b == a { continue; }
            for c in 0..5 {
                if c == a || c == b { continue; }
                for d in 0..5 {
                    if d == a || d == b || d == c { continue; }
                    for e in 0..5 {
                        if e == a || e == b || e == c || e == d { continue; }

                        let phase_settings = vec![a, b, c, d, e];
                        let output = run_amplifiers(&program, &phase_settings);

                        if output > max_output {
                            max_output = output;
                        }
                    }
                }
            }
        }
    }

    println!("{}", max_output);
}
