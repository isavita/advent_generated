
import os

fn solve(grid []string) int {
    rows := grid.len
    if rows == 0 { return 0 }
    cols := grid[0].len

    mut visited := [][]bool{len: rows, init: []bool{len: cols}}
    mut total_price := 0

    for r in 0 .. rows {
        for c in 0 .. cols {
            if !visited[r][c] {
                area, perimeter := calc_region(grid, r, c, mut visited)
                total_price += area * perimeter
            }
        }
    }
    return total_price
}

fn calc_region(grid []string, row int, col int, mut visited [][]bool) (int, int) {
    rows := grid.len
    cols := grid[0].len
    ch := grid[row][col]
    mut area := 0
    mut perimeter := 0

    mut queue := [[row, col]]
    visited[row][col] = true

    for queue.len > 0 {
        x, y := queue[0][0], queue[0][1]
        queue = queue[1..]
        area++
        is_border := x == 0 || x == rows - 1 || y == 0 || y == cols - 1

        neighbors := [[x-1,y], [x+1,y], [x,y-1], [x,y+1]]
        for n in neighbors {
            nx, ny := n[0], n[1]
            if nx >= 0 && nx < rows && ny >= 0 && ny < cols {
                if grid[nx][ny] != ch {
                    perimeter++
                } else if !visited[nx][ny] {
                    queue << [nx, ny]
                    visited[nx][ny] = true
                }
            } else if is_border {
                perimeter++
            }
        }
    }
    return area, perimeter
}

fn main() {
    content := os.read_file('input.txt') or { panic('cannot read input.txt') }
    grid := content.split_into_lines()
    println(solve(grid))
}
