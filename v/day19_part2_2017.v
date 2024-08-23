import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    diagram := input.split('\n').filter(it.len > 0)
    mut x := diagram[0].index('|') or { -1 }
    mut y := 0
    mut direction := [0, 1]
    mut letters := ''
    mut steps := 0

    for {
        if x < 0 || x >= diagram[y].len || y < 0 || y >= diagram.len || diagram[y][x] == ` ` {
            break
        }
        steps++
        current := diagram[y][x]
        if current.is_letter() {
            letters += current.ascii_str()
        } else if current == `+` {
            direction = if direction[0] != 0 {
                if y > 0 && diagram[y - 1][x] != ` ` { [0, -1] } else { [0, 1] }
            } else {
                if x > 0 && diagram[y][x - 1] != ` ` { [-1, 0] } else { [1, 0] }
            }
        }
        x += direction[0]
        y += direction[1]
    }
    println('Letters: $letters')
    println('Steps: $steps')
}