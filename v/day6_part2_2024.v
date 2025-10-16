
import os

fn loops(grid [][]u8, sx int, sy int, sdir int, h int, w int) bool {
    mut x := sx
    mut y := sy
    mut dir := sdir
    mut seen := []bool{len: h * w * 4, init: false}
    dirs := [[0, -1], [1, 0], [0, 1], [-1, 0]]
    for step := 0; step < 2000000; step++ {
        state_index := (y * w + x) * 4 + dir
        if seen[state_index] {
            return true
        }
        seen[state_index] = true
        dx := dirs[dir][0]
        dy := dirs[dir][1]
        nx := x + dx
        ny := y + dy
        if nx < 0 || nx >= w || ny < 0 || ny >= h {
            return false
        }
        if grid[ny][nx] == `#` {
            dir = (dir + 1) % 4
            continue
        }
        x = nx
        y = ny
    }
    return false
}

fn main() {
    content := os.read_file('input.txt') or { panic(err) }
    lines := content.split_into_lines()
    mut grid := [][]u8{len: lines.len}
    for i, line in lines {
        grid[i] = line.bytes()
    }
    h := grid.len
    w := grid[0].len
    mut start_x := 0
    mut start_y := 0
    mut start_dir := 0
    for y in 0 .. h {
        for x in 0 .. w {
            match grid[y][x] {
                `^` {
                    start_x = x
                    start_y = y
                    start_dir = 0
                }
                `>` {
                    start_x = x
                    start_y = y
                    start_dir = 1
                }
                `v` {
                    start_x = x
                    start_y = y
                    start_dir = 2
                }
                `<` {
                    start_x = x
                    start_y = y
                    start_dir = 3
                }
                else {}
            }
        }
    }
    grid[start_y][start_x] = `.`
    mut can_loop := 0
    for y in 0 .. h {
        for x in 0 .. w {
            if x == start_x && y == start_y {
                continue
            }
            if grid[y][x] != `.` {
                continue
            }
            grid[y][x] = `#`
            if loops(grid, start_x, start_y, start_dir, h, w) {
                can_loop++
            }
            grid[y][x] = `.`
        }
    }
    println(can_loop)
}
