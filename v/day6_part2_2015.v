import os

const grid_size = 1000

fn main() {
	mut grid := [][]int{len: grid_size, init: []int{len: grid_size, init: 0}}
	
	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		process_instruction(line, mut grid)
	}

	println(total_brightness(grid))
}

fn process_instruction(instruction string, mut grid [][]int) {
	parts := instruction.split(' ')
	start := parts[parts.len - 3].split(',')
	end := parts[parts.len - 1].split(',')
	start_x, start_y := start[0].int(), start[1].int()
	end_x, end_y := end[0].int(), end[1].int()

	for x in start_x..end_x + 1 {
		for y in start_y..end_y + 1 {
			if instruction.starts_with('turn on') {
				grid[x][y]++
			} else if instruction.starts_with('turn off') {
				if grid[x][y] > 0 {
					grid[x][y]--
				}
			} else if instruction.starts_with('toggle') {
				grid[x][y] += 2
			}
		}
	}
}

fn total_brightness(grid [][]int) int {
	mut brightness := 0
	for row in grid {
		for light in row {
			brightness += light
		}
	}
	return brightness
}