import os

fn main() {
    input := os.read_file('input.txt') or { panic(err) }
    grid := parse_input(input)
    max_score := find_max_scenic_score(grid)
    println(max_score)
}

fn parse_input(input string) [][]int {
    return input.split('\n').filter(it != '').map(fn (line string) []int {
        return line.split('').map(it.int())
    })
}

fn find_max_scenic_score(grid [][]int) int {
    mut max_score := 0
    for i in 0 .. grid.len {
        for j in 0 .. grid[i].len {
            score := calculate_scenic_score(grid, i, j)
            if score > max_score {
                max_score = score
            }
        }
    }
    return max_score
}

fn calculate_scenic_score(grid [][]int, row int, col int) int {
    up := count_visible_trees(grid, row, col, -1, 0)
    down := count_visible_trees(grid, row, col, 1, 0)
    left := count_visible_trees(grid, row, col, 0, -1)
    right := count_visible_trees(grid, row, col, 0, 1)
    return up * down * left * right
}

fn count_visible_trees(grid [][]int, row int, col int, row_step int, col_step int) int {
    mut count := 0
    height := grid[row][col]
    mut r := row + row_step
    mut c := col + col_step
    for r >= 0 && r < grid.len && c >= 0 && c < grid[0].len {
        count++
        if grid[r][c] >= height {
            break
        }
        r += row_step
        c += col_step
    }
    return count
}