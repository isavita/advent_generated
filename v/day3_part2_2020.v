import os

fn main() {
    lines := os.read_lines('input.txt') or { return }

    slopes := [
        [1, 1],
        [3, 1],
        [5, 1],
        [7, 1],
        [1, 2],
    ]

    mut product := 1
    for slope in slopes {
        mut tree_count := 0
        mut pos := 0
        for i in 0..lines.len {
            if i % slope[1] == 0 {
                if lines[i][pos % lines[i].len] == `#` {
                    tree_count++
                }
                pos += slope[0]
            }
        }
        product *= tree_count
    }
    println(product)
}