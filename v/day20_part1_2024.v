
import os

struct Pos {
    r int
    c int
}

fn bfs(start Pos, h int, w int, walls [][]bool) [][]int {
    mut dist := [][]int{len: h, init: []int{len: w, init: -1}}
    mut q := []Pos{}
    dist[start.r][start.c] = 0
    q << start
    dr := [1, -1, 0, 0]
    dc := [0, 0, 1, -1]
    for q.len > 0 {
        curr := q[0]
        q = q[1..]
        for i := 0; i < 4; i++ {
            nr := curr.r + dr[i]
            nc := curr.c + dc[i]
            if nr >= 0 && nr < h && nc >= 0 && nc < w {
                if walls[nr][nc] { continue }
                if dist[nr][nc] == -1 {
                    dist[nr][nc] = dist[curr.r][curr.c] + 1
                    q << Pos{nr, nc}
                }
            }
        }
    }
    return dist
}

fn is_track(r int, c int, h int, w int, walls [][]bool) bool {
    return r >= 0 && r < h && c >= 0 && c < w && !walls[r][c]
}

fn main() {
    content := os.read_file('input.txt') or { return }
    grid := content.split_into_lines()
    h := grid.len
    if h == 0 { println(0); return }
    w := grid[0].len
    if w == 0 { println(0); return }
    mut s := Pos{}
    mut e := Pos{}
    mut walls := [][]bool{len: h, init: []bool{len: w}}
    mut track_cells := []Pos{}
    for r := 0; r < h; r++ {
        for c := 0; c < w; c++ {
            ch := grid[r][c]
            if ch == `S` { s = Pos{r, c} }
            else if ch == `E` { e = Pos{r, c} }
            if ch == `#` { walls[r][c] = true }
            else { track_cells << Pos{r, c} }
        }
    }
    dist_from_s := bfs(s, h, w, walls)
    dist_from_e := bfs(e, h, w, walls)
    if dist_from_s[e.r][e.c] == -1 { println(0); return }
    normal_cost := dist_from_s[e.r][e.c]
    mut possible_cheats := 0
    dr_dir := [1, -1, 0, 0]
    dc_dir := [0, 0, 1, -1]
    for start_pos in track_cells {
        sd := dist_from_s[start_pos.r][start_pos.c]
        if sd == -1 { continue }
        for i := 0; i < 4; i++ {
            m1r := start_pos.r + dr_dir[i]
            m1c := start_pos.c + dc_dir[i]
            if !(m1r >= 0 && m1r < h && m1c >= 0 && m1c < w) { continue }
            for j := 0; j < 4; j++ {
                m2r := m1r + dr_dir[j]
                m2c := m1c + dc_dir[j]
                if !is_track(m2r, m2c, h, w, walls) { continue }
                ed := dist_from_e[m2r][m2c]
                if ed == -1 { continue }
                new_cost := sd + 2 + ed
                saving := normal_cost - new_cost
                if saving >= 100 { possible_cheats++ }
            }
        }
    }
    println(possible_cheats)
}
