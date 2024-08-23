import os

fn main() {
    grid := read_input('input.txt')
    visible_count := count_visible_trees(grid)
    println(visible_count)
}

fn read_input(filename string) [][]int {
    mut grid := [][]int{}
    lines := os.read_lines(filename) or { panic(err) }
    for line in lines {
        mut row := []int{}
        for ch in line {
            row << ch.ascii_str().int()
        }
        grid << row
    }
    return grid
}

fn count_visible_trees(grid [][]int) int {
    rows := grid.len
    cols := grid[0].len
    mut visible := 0

    for i in 0 .. rows {
        for j in 0 .. cols {
            if is_visible(grid, i, j) {
                visible++
            }
        }
    }
    return visible
}

fn is_visible(grid [][]int, row int, col int) bool {
    if row == 0 || row == grid.len - 1 || col == 0 || col == grid[0].len - 1 {
        return true
    }
    height := grid[row][col]
    return check_direction(grid, row, col, height, -1, 0) || // up
           check_direction(grid, row, col, height, 1, 0) ||  // down
           check_direction(grid, row, col, height, 0, -1) || // left
           check_direction(grid, row, col, height, 0, 1)     // right
}

fn check_direction(grid [][]int, row int, col int, height int, d_row int, d_col int) bool {
    mut r := row + d_row
    mut c := col + d_col
    for r >= 0 && r < grid.len && c >= 0 && c < grid[0].len {
        if grid[r][c] >= height {
            return false
        }
        r += d_row
        c += d_col
    }
    return true
}