import os

fn main() {
    mut count := 0
    lines := os.read_lines('input.txt') or { return }
    
    for line in lines {
        sides := line.split(' ').filter(it != '').map(it.int())
        if sides.len != 3 { continue }
        if is_valid_triangle(sides) {
            count++
        }
    }
    println(count)
}

fn is_valid_triangle(sides []int) bool {
    return sides[0] + sides[1] > sides[2] &&
           sides[0] + sides[2] > sides[1] &&
           sides[1] + sides[2] > sides[0]
}