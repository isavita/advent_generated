import os

fn main() {
    mut grid := [][]int{}
    input := os.read_file('input.txt') or { panic(err) }
    for line in input.split('\n') {
        if line.trim_space() != '' {
            grid << line.split('').map(it.int())
        }
    }

    total_flashes := simulate_flashes(mut grid, 100)
    println(total_flashes)
}

fn simulate_flashes(mut grid [][]int, steps int) int {
    mut total_flashes := 0
    for _ in 0 .. steps {
        mut flashed := [][]bool{len: grid.len, init: []bool{len: grid[0].len, init: false}}
        for y in 0 .. grid.len {
            for x in 0 .. grid[y].len {
                grid[y][x]++
            }
        }
        for y in 0 .. grid.len {
            for x in 0 .. grid[y].len {
                if grid[y][x] > 9 && !flashed[y][x] {
                    flash(mut grid, mut flashed, y, x)
                }
            }
        }
        for y in 0 .. grid.len {
            for x in 0 .. grid[y].len {
                if flashed[y][x] {
                    grid[y][x] = 0
                    total_flashes++
                }
            }
        }
    }
    return total_flashes
}

fn flash(mut grid [][]int, mut flashed [][]bool, y int, x int) {
    flashed[y][x] = true
    for dy in -1 .. 2 {
        for dx in -1 .. 2 {
            ny, nx := y + dy, x + dx
            if ny >= 0 && ny < grid.len && nx >= 0 && nx < grid[0].len && !(dy == 0 && dx == 0) {
                grid[ny][nx]++
                if grid[ny][nx] > 9 && !flashed[ny][nx] {
                    flash(mut grid, mut flashed, ny, nx)
                }
            }
        }
    }
}