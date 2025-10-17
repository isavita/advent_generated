
import os

fn main() {
    lines := os.read_lines('input.txt') or { panic('missing input.txt') }
    mut grid := [][]u8{len: lines.len, init: []u8{len: lines[0].len}}
    for y, l in lines {
        for x, c in l {
            grid[y][x] = c
        }
    }
    mut sx, mut sy := -1, -1
    for y, row in grid {
        for x, c in row {
            if c == `S` {
                sx, sy = x, y
                break
            }
        }
        if sx >= 0 { break }
    }
    mut dx, mut dy := 0, 0
    if sy > 0 && grid[sy-1][sx] in [`|`, `7`, `F`] { dx, dy = 0, -1 }
    else if sx+1 < grid[0].len && grid[sy][sx+1] in [`-`, `7`, `J`] { dx, dy = 1, 0 }
    else if sy+1 < grid.len && grid[sy+1][sx] in [`|`, `L`, `J`] { dx, dy = 0, 1 }
    else { dx, dy = -1, 0 }
    mut steps := 0
    mut x, mut y := sx, sy
    for {
        steps++
        x += dx
        y += dy
        if x == sx && y == sy { break }
        c := grid[y][x]
        if c == `|` { dy = if dy == 1 { 1 } else { -1 } }
        else if c == `-` { dx = if dx == 1 { 1 } else { -1 } }
        else if c == `L` { dx, dy = if dy == 1 { 1, 0 } else { 0, -1 } }
        else if c == `J` { dx, dy = if dy == 1 { -1, 0 } else { 0, -1 } }
        else if c == `7` { dx, dy = if dy == -1 { -1, 0 } else { 0, 1 } }
        else if c == `F` { dx, dy = if dy == -1 { 1, 0 } else { 0, 1 } }
    }
    println(steps / 2)
}
