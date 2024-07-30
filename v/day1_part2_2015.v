import os

fn main() {
    input := os.read_file('input.txt') or { panic('Failed to read file') }
    instructions := input.trim_space()
    
    mut floor := 0
    mut basement_position := -1

    for i, ch in instructions {
        floor += if ch == `(` { 1 } else { -1 }
        if floor == -1 && basement_position == -1 {
            basement_position = i + 1
        }
    }

    println('Final floor: $floor')
    println('First basement position: $basement_position')
}