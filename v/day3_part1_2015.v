import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    mut visited := map[string]int{}
    mut x, mut y := 0, 0

    visited['0,0'] = 1

    for direction in input.trim_space() {
        match direction {
            `^` { y++ }
            `v` { y-- }
            `>` { x++ }
            `<` { x-- }
            else {}
        }
        visited['$x,$y'] = 1
    }

    println(visited.len)
}