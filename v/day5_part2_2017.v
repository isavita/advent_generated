import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    mut jumps := input.split('\n').filter(it != '')
        .map(it.int()).clone()

    mut steps := 0
    mut position := 0

    for position >= 0 && position < jumps.len {
        offset := jumps[position]
        jumps[position] += if offset >= 3 { -1 } else { 1 }
        position += offset
        steps++
    }

    println(steps)
}