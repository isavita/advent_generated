import os

fn main() {
    input := os.read_file('input.txt') or {
        eprintln('Failed to read file: $err')
        return
    }
    
    digits := input.trim_space()
    mut sum := 0
    
    for i in 0 .. digits.len {
        next_index := (i + 1) % digits.len
        if digits[i] == digits[next_index] {
            sum += digits[i].ascii_str().int()
        }
    }
    
    println(sum)
}