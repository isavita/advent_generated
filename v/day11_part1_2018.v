import os

fn calculate_power_level(x int, y int, serial_number int) int {
	rack_id := x + 10
	mut power_level := rack_id * y
	power_level += serial_number
	power_level *= rack_id
	power_level = (power_level / 100) % 10
	return power_level - 5
}

fn main() {
	serial_number := read_serial_number('input.txt')
	grid := create_grid(serial_number)
	best_coord := find_best_square(grid)
	println('${best_coord[0]},${best_coord[1]}')
}

fn read_serial_number(filename string) int {
	return os.read_file(filename) or { panic(err) }.trim_space().int()
}

fn create_grid(serial_number int) [][]int {
	mut grid := [][]int{len: 300, init: []int{len: 300}}
	for y in 0 .. 300 {
		for x in 0 .. 300 {
			grid[y][x] = calculate_power_level(x + 1, y + 1, serial_number)
		}
	}
	return grid
}

fn find_best_square(grid [][]int) []int {
	mut max_power := -99999
	mut best_coord := []int{len: 2}
	for y in 0 .. 298 {
		for x in 0 .. 298 {
			mut total_power := 0
			for dy in 0 .. 3 {
				for dx in 0 .. 3 {
					total_power += grid[y + dy][x + dx]
				}
			}
			if total_power > max_power {
				max_power = total_power
				best_coord = [x + 1, y + 1]
			}
		}
	}
	return best_coord
}