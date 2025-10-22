
import os

fn main() {
    lines := os.read_lines('input.txt') or { panic('missing input.txt') }
    mut grid := [][]u8{cap: lines.len}
    for line in lines {
        grid << line.bytes()
    }

    mut step := 0
    for {
        mut moved := false
        h := grid.len
        w := grid[0].len

        // east
        mut can := [][]bool{len: h, init: []bool{len: w, init: false}}
        for y in 0 .. h {
            for x in 0 .. w {
                if grid[y][x] == `>` && grid[y][(x + 1) % w] == `.` {
                    can[y][x] = true
                }
            }
        }
        for y in 0 .. h {
            for x in 0 .. w {
                if can[y][x] {
                    grid[y][x] = `.`
                    grid[y][(x + 1) % w] = `>`
                    moved = true
                }
            }
        }

        // south
        for y in 0 .. h {
            for x in 0 .. w {
                can[y][x] = grid[y][x] == `v` && grid[(y + 1) % h][x] == `.`
            }
        }
        for y in 0 .. h {
            for x in 0 .. w {
                if can[y][x] {
                    grid[y][x] = `.`
                    grid[(y + 1) % h][x] = `v`
                    moved = true
                }
            }
        }

        step++
        if !moved {
            break
        }
    }
    println(step)
}
