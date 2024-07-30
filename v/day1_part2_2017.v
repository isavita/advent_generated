import os

fn read_input(file string) []int {
    content := os.read_file(file) or { panic(err) }
    return content.split('').map(it.int())
}

fn part_one(digits []int) int {
    mut sum := 0
    for i in 0 .. digits.len {
        if digits[i] == digits[(i + 1) % digits.len] {
            sum += digits[i]
        }
    }
    return sum
}

fn part_two(digits []int) int {
    half_len := digits.len / 2
    mut sum := 0
    for i in 0 .. digits.len {
        if digits[i] == digits[(i + half_len) % digits.len] {
            sum += digits[i]
        }
    }
    return sum
}

fn main() {
    digits := read_input('input.txt')
    println('Part One: ${part_one(digits)}')
    println('Part Two: ${part_two(digits)}')
}