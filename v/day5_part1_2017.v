import os

fn main() {
    offsets := os.read_lines('input.txt') or { panic(err) }
    mut jumps := offsets.map(it.int())
    mut position := 0
    mut steps := 0

    for position >= 0 && position < jumps.len {
        jump := jumps[position]
        jumps[position] += 1
        position += jump
        steps++
    }

    println(steps)
}