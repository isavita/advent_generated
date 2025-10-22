
import os

fn main() {
    mut grid := read_input('input.txt')
    mut step := 0
    for {
        step++
        flashes := simulate_step(mut grid)
        if flashes == 100 {
            break
        }
    }
    println(step)
}

fn read_input(filename string) [][]int {
    content := os.read_file(filename) or { panic(err) }
    mut grid := [][]int{}
    for line in content.split_into_lines() {
        mut row := []int{}
        for ch in line {
            row << int(ch - `0`)
        }
        grid << row
    }
    return grid
}

fn simulate_step(mut grid [][]int) int {
    mut flashes := 0
    mut flashed := map[string]bool{}
    h := grid.len
    w := grid[0].len
    for y in 0 .. h {
        for x in 0 .. w {
            grid[y][x]++
        }
    }
    for y in 0 .. h {
        for x in 0 .. w {
            if grid[y][x] > 9 {
                flashes += flash(mut grid, x, y, mut flashed)
            }
        }
    }
    for k, _ in flashed {
        xy := k.split(',')
        y := xy[1].int()
        x := xy[0].int()
        grid[y][x] = 0
    }
    return flashes
}

fn flash(mut grid [][]int, x int, y int, mut flashed map[string]bool) int {
    key := '${x},${y}'
    if flashed[key] {
        return 0
    }
    flashed[key] = true
    mut count := 1
    dirs := [
        [1, 0], [-1, 0], [0, 1], [0, -1],
        [1, 1], [-1, -1], [1, -1], [-1, 1],
    ]
    h := grid.len
    w := grid[0].len
    for d in dirs {
        nx := x + d[0]
        ny := y + d[1]
        if nx >= 0 && nx < w && ny >= 0 && ny < h {
            grid[ny][nx]++
            if grid[ny][nx] > 9 {
                count += flash(mut grid, nx, ny, mut flashed)
            }
        }
    }
    return count
}
