
import os

const max_lines = 200
const max_width = 200
const max_galaxies = max_lines * max_width
const expansion_factor = 1_000_000

struct Coord {
    x int
    y int
}

fn abs_diff(a i64, b i64) i64 {
    return if a > b { a - b } else { b - a }
}

fn main() {
    content := os.read_file('input.txt') or { panic(err) }
    lines := content.split_into_lines()

    mut galaxies := []Coord{cap: max_galaxies}
    mut row_has := [false].repeat(max_lines)
    mut col_has := [false].repeat(max_width)

    for y, line in lines {
        for x, ch in line {
            if ch == `#` {
                galaxies << Coord{x, y}
                row_has[y] = true
                col_has[x] = true
            }
        }
    }

    mut row_off := [0].repeat(max_lines)
    mut cur := 0
    for y in 0 .. lines.len {
        row_off[y] = cur
        if !row_has[y] {
            cur++
        }
    }

    mut col_off := [0].repeat(max_width)
    cur = 0
    for x in 0 .. lines[0].len {
        col_off[x] = cur
        if !col_has[x] {
            cur++
        }
    }

    mut total := i64(0)
    delta := expansion_factor - 1
    for i := 0; i < galaxies.len - 1; i++ {
        g1 := galaxies[i]
        x1 := i64(g1.x) + i64(col_off[g1.x]) * delta
        y1 := i64(g1.y) + i64(row_off[g1.y]) * delta
        for j := i + 1; j < galaxies.len; j++ {
            g2 := galaxies[j]
            x2 := i64(g2.x) + i64(col_off[g2.x]) * delta
            y2 := i64(g2.y) + i64(row_off[g2.y]) * delta
            total += abs_diff(x1, x2) + abs_diff(y1, y2)
        }
    }

    println(total)
}
