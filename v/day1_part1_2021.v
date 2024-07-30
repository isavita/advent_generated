import os

fn main() {
    depths := os.read_lines('input.txt') or { panic('Failed to read file') }
    mut count := 0

    for i in 1 .. depths.len {
        if depths[i].int() > depths[i - 1].int() {
            count++
        }
    }

    println(count)
}