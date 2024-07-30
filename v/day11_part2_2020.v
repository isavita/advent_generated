module main

import os

struct Point {
	x int
	y int
}

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
					if count_visible_occupied(seating_area, i, j) == 0 {
						new_seating_area[i][j] = `#`
						stabilized = false
					}
				}
				`#` {
					if count_visible_occupied(seating_area, i, j) >= 5 {
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

fn count_visible_occupied(seating_area [][]rune, row int, col int) int {
	mut count := 0
	directions := [Point{-1, -1}, Point{0, -1}, Point{1, -1}, Point{-1, 0}, Point{1, 0}, Point{-1, 1}, Point{0, 1}, Point{1, 1}]

	for dir in directions {
		mut r := row + dir.y
		mut c := col + dir.x
		for r >= 0 && r < seating_area.len && c >= 0 && c < seating_area[0].len {
			match seating_area[r][c] {
				`L` { break }
				`#` {
					count++
					break
				}
				else {}
			}
			r += dir.y
			c += dir.x
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