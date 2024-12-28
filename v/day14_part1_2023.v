
import os

fn main() {
	lines := os.read_lines('input.txt') or { panic(err) }
	mut grid := lines.map(it.split(''))

	tilt_north(mut grid)

	load := calculate_load(grid)
	println(load)
}

fn tilt_north(mut grid [][]string) {
	for col in 0 .. grid[0].len {
		mut next_row := 0
		for row in 0 .. grid.len {
			match grid[row][col] {
				'O' {
					if row != next_row {
						grid[next_row][col] = 'O'
						grid[row][col] = '.'
					}
					next_row++
				}
				'#' {
					next_row = row + 1
				}
				else {}
			}
		}
	}
}

fn calculate_load(grid [][]string) int {
	mut load := 0
	for row in 0 .. grid.len {
		for col in 0 .. grid[row].len {
			if grid[row][col] == 'O' {
				load += grid.len - row
			}
		}
	}
	return load
}
