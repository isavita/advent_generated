import os

fn main() {
    forest := os.read_lines('input.txt') or { panic(err) }
    trees := count_trees(forest, 3, 1)
    println(trees)
}

fn count_trees(forest []string, right int, down int) int {
    mut trees := 0
    mut x := 0
    width := forest[0].len

    for y in 0 .. forest.len {
        if y % down == 0 {
            if forest[y][x % width] == `#` {
                trees++
            }
            x += right
        }
    }
    return trees
}