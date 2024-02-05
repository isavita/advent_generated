fn is_nice(s: &str) -> bool {
    let mut vowels = 0;
    let mut double_letter = false;
    let mut prev_char = ' ';

    for c in s.chars() {
        match c {
            'a' | 'e' | 'i' | 'o' | 'u' => vowels += 1,
            _ => {}
        }

        if c == prev_char {
            double_letter = true;
        }

        match (prev_char, c) {
            ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') => return false,
            _ => {}
        }

        prev_char = c;
    }

    vowels >= 3 && double_letter
}

fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let nice_count = input.lines().filter(|&line| is_nice(line)).count();
    println!("{}", nice_count);
}