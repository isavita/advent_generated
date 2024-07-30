import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    parts := input.split('-')
    start := parts[0].int()
    end := parts[1].int()
    
    mut count := 0

    for num in start..end + 1 {
        if is_valid_password(num) {
            count++
        }
    }
    
    println(count)
}

fn is_valid_password(num int) bool {
    digits := num.str().split('')
    mut has_double := false
    for i in 0..digits.len - 1 {
        if digits[i] > digits[i + 1] {
            return false
        }
        if digits[i] == digits[i + 1] {
            has_double = true
        }
    }
    return has_double
}