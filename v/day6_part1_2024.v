
import os

fn main() {
    mut grid := [][]u8{}
    mut h := 0
    mut w := 0
    lines := os.read_lines('input.txt') or { return }
    for line in lines {
        grid << line.bytes()
    }
    h = grid.len
    w = grid[0].len
    dirs := [[0,-1],[1,0],[0,1],[-1,0]]!
    mut x := 0
    mut y := 0
    mut dir_idx := 0
    for i := 0; i < h; i++ {
        for j := 0; j < w; j++ {
            match grid[i][j] {
                `^` { x = j; y = i; dir_idx = 0 }
                `>` { x = j; y = i; dir_idx = 1 }
                `v` { x = j; y = i; dir_idx = 2 }
                `<` { x = j; y = i; dir_idx = 3 }
                else {}
            }
        }
    }
    mut dir_x := dirs[dir_idx][0]
    mut dir_y := dirs[dir_idx][1]
    mut visited := [][]bool{len: h, init: []bool{len: w, init: false}}
    mut count := 0
    visited[y][x] = true
    count++
    for {
        nx := x + dir_x
        ny := y + dir_y
        if nx < 0 || nx >= w || ny < 0 || ny >= h {
            break
        }
        if grid[ny][nx] == `#` {
            dir_idx = (dir_idx + 1) % 4
            dir_x = dirs[dir_idx][0]
            dir_y = dirs[dir_idx][1]
            continue
        }
        x = nx
        y = ny
        if !visited[y][x] {
            visited[y][x] = true
            count++
        }
    }
    println(count)
}
