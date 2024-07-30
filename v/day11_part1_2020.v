import os

fn main() {
	mut seating_area := read_input('input.txt')
	mut stabilized := false
	for !stabilized {
		seating_area, stabilized = simulate_seating(seating_area)
	}
	println(count_occupied_seats(seating_area))
}

fn read_input(filename string) [][]rune {
	mut seating_area := [][]rune{}
	lines := os.read_lines(filename) or { return seating_area }
	for line in lines {
		seating_area << line.runes()
	}
	return seating_area
}

fn simulate_seating(seating_area [][]rune) ([][]rune, bool) {
	rows := seating_area.len
	cols := seating_area[0].len
	mut new_seating_area := seating_area.clone()
	mut stabilized := true

	for i in 0 .. rows {
		for j in 0 .. cols {
			match seating_area[i][j] {
				`L` {
					if count_adjacent_occupied(seating_area, i, j) == 0 {
						new_seating_area[i][j] = `#`
						stabilized = false
					}
				}
				`#` {
					if count_adjacent_occupied(seating_area, i, j) >= 4 {
						new_seating_area[i][j] = `L`
						stabilized = false
					}
				}
				else {}
			}
		}
	}
	return new_seating_area, stabilized
}

fn count_adjacent_occupied(seating_area [][]rune, row int, col int) int {
	mut count := 0
	for i in row - 1 .. row + 2 {
		for j in col - 1 .. col + 2 {
			if i == row && j == col {
				continue
			}
			if i >= 0 && i < seating_area.len && j >= 0 && j < seating_area[0].len {
				if seating_area[i][j] == `#` {
					count++
				}
			}
		}
	}
	return count
}

fn count_occupied_seats(seating_area [][]rune) int {
	mut count := 0
	for row in seating_area {
		for seat in row {
			if seat == `#` {
				count++
			}
		}
	}
	return count
}