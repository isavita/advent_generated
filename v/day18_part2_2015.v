module main

import os

const grid_size = 100
const steps = 100

fn count_on_neighbors(grid [][]bool, x int, y int) int {
	mut on := 0
	for dx in [-1, 0, 1] {
		for dy in [-1, 0, 1] {
			if dx == 0 && dy == 0 {
				continue
			}
			nx := x + dx
			ny := y + dy
			if nx >= 0 && nx < grid_size && ny >= 0 && ny < grid_size && grid[nx][ny] {
				on++
			}
		}
	}
	return on
}

fn step(grid [][]bool) [][]bool {
	mut new_grid := [][]bool{len: grid_size, init: []bool{len: grid_size}}
	for x in 0 .. grid_size {
		for y in 0 .. grid_size {
			on_neighbors := count_on_neighbors(grid, x, y)
			if grid[x][y] {
				new_grid[x][y] = on_neighbors in [2, 3]
			} else {
				new_grid[x][y] = on_neighbors == 3
			}
		}
	}
	new_grid[0][0] = true
	new_grid[0][grid_size - 1] = true
	new_grid[grid_size - 1][0] = true
	new_grid[grid_size - 1][grid_size - 1] = true
	return new_grid
}

fn main() {
	mut grid := [][]bool{len: grid_size, init: []bool{len: grid_size}}
	lines := os.read_lines('input.txt') or { panic(err) }
	for y in 0 .. lines.len {
		for x in 0 .. lines[y].len {
			grid[x][y] = lines[y][x] == `#`
		}
	}
	grid[0][0] = true
	grid[0][grid_size - 1] = true
	grid[grid_size - 1][0] = true
	grid[grid_size - 1][grid_size - 1] = true
	for _ in 0 .. steps {
		grid = step(grid)
	}
	mut on_count := 0
	for row in grid {
		for light in row {
			if light {
				on_count++
			}
		}
	}
	println(on_count)
}