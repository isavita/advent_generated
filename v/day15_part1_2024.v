
import os

fn push_boxes(mut grid [][]u8, r int, c int, dr int, dc int) bool {
    nr := r + dr
    nc := c + dc
    if nr < 0 || nr >= grid.len || nc < 0 || nc >= grid[0].len {
        return false
    }
    if grid[nr][nc] == `#` {
        return false
    }
    if grid[nr][nc] == `O` {
        if !push_boxes(mut grid, nr, nc, dr, dc) {
            return false
        }
    }
    if grid[nr][nc] == `.` {
        grid[nr][nc] = `O`
        grid[r][c] = `.`
        return true
    }
    return false
}

fn main() {
    content := os.read_file('input.txt') or { panic('cannot read input.txt') }
    lines := content.split_into_lines()
    mut grid := [][]u8{}
    mut moves := []u8{}
    mut robot_r := 0
    mut robot_c := 0
    mut reading_map := true
    for line in lines {
        if line.len == 0 {
            if grid.len > 0 {
                reading_map = false
            }
            continue
        }
        if reading_map {
            if line.contains('#') || line.contains('@') || line.contains('O') || line.contains('.') {
                grid << line.bytes()
                if idx := line.index('@') {
                    robot_r = grid.len - 1
                    robot_c = idx
                }
            } else {
                reading_map = false
                moves << line.bytes()
            }
        } else {
            moves << line.bytes()
        }
    }
    for m in moves {
        mut dr := 0
        mut dc := 0
        match m {
            `^` { dr = -1 }
            `v` { dr = 1 }
            `<` { dc = -1 }
            `>` { dc = 1 }
            else { continue }
        }
        nr := robot_r + dr
        nc := robot_c + dc
        if nr < 0 || nr >= grid.len || nc < 0 || nc >= grid[0].len {
            continue
        }
        next := grid[nr][nc]
        if next == `#` {
            continue
        }
        if next == `O` {
            if !push_boxes(mut grid, nr, nc, dr, dc) {
                continue
            }
        }
        grid[robot_r][robot_c] = `.`
        grid[nr][nc] = `@`
        robot_r = nr
        robot_c = nc
    }
    mut total := 0
    for r in 0 .. grid.len {
        for c in 0 .. grid[r].len {
            if grid[r][c] == `O` {
                total += r * 100 + c
            }
        }
    }
    println(total)
}
