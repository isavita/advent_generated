
import os

fn check_mas(grid [][]u8, x int, y int, dx int, dy int) bool {
    if x < 0 || y < 0 || x >= grid.len || y >= grid[0].len { return false }
    word := 'MAS'
    mut forward := true
    mut backward := true
    for i := 0; i < 3; i++ {
        nx := x + dx * i
        ny := y + dy * i
        if nx < 0 || ny < 0 || nx >= grid.len || ny >= grid[0].len || grid[nx][ny] != word[i] { forward = false }
        if nx < 0 || ny < 0 || nx >= grid.len || ny >= grid[0].len || grid[nx][ny] != word[2 - i] { backward = false }
    }
    return forward || backward
}

fn check_xmas(grid [][]u8, x int, y int) bool {
    return (check_mas(grid, x - 1, y - 1, 1, 1) && check_mas(grid, x - 1, y + 1, 1, -1)) ||
           (check_mas(grid, x + 1, y - 1, -1, 1) && check_mas(grid, x + 1, y + 1, -1, -1))
}

fn count_xmas_patterns(grid [][]u8) int {
    if grid.len < 3 || grid[0].len < 3 { return 0 }
    mut count := 0
    for i := 1; i < grid.len - 1; i++ {
        for j := 1; j < grid[0].len - 1; j++ {
            if grid[i][j] == `A` && check_xmas(grid, i, j) { count++ }
        }
    }
    return count
}

fn main() {
    content := os.read_file('input.txt') or { panic(err) }
    lines := content.split_into_lines()
    mut grid := [][]u8{len: lines.len, init: []u8{len: lines[0].len}}
    for i, line in lines {
        for j, ch in line { grid[i][j] = ch }
    }
    println(count_xmas_patterns(grid))
}
