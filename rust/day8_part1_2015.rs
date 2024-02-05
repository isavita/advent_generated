use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let mut code_chars = 0;
    let mut memory_chars = 0;

    for line in input.lines() {
        code_chars += line.len();

        let mut iter = line.chars().skip(1).take(line.len() - 2);
        let mut memory_len = 0;

        while let Some(c) = iter.next() {
            if c == '\\' {
                match iter.next().unwrap() {
                    '\\' => memory_len += 1,
                    '"' => memory_len += 1,
                    'x' => {
                        iter.next();
                        iter.next();
                        memory_len += 1;
                    },
                    _ => (),
                }
            } else {
                memory_len += 1;
            }
        }

        memory_chars += memory_len;
    }

    println!("{}", code_chars - memory_chars);
}