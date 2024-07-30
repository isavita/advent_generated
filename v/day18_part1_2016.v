module main

import os

const total_rows = 40

fn main() {
	first_row := read_first_row('input.txt')
	safe_tiles_count := count_safe_tiles(first_row, total_rows)
	println(safe_tiles_count)
}

fn read_first_row(filename string) string {
	return os.read_file(filename) or { panic(err) }
}

fn count_safe_tiles(first_row string, total_rows int) int {
	mut current_row := first_row
	mut safe_count := count_char(current_row, `.`)

	for _ in 1 .. total_rows {
		mut next_row := ''
		for j in 0 .. current_row.len {
			if is_trap(j - 1, j, j + 1, current_row) {
				next_row += '^'
			} else {
				next_row += '.'
				safe_count++
			}
		}
		current_row = next_row
	}
	return safe_count
}

fn is_trap(left int, center int, right int, row string) bool {
	l := safe_if_out_of_bounds(left, row)
	c := row[center]
	r := safe_if_out_of_bounds(right, row)

	return (l == `^` && c == `^` && r == `.`) ||
		(c == `^` && r == `^` && l == `.`) ||
		(l == `^` && c == `.` && r == `.`) ||
		(r == `^` && c == `.` && l == `.`)
}

fn safe_if_out_of_bounds(index int, row string) u8 {
	if index < 0 || index >= row.len {
		return `.` 
	}
	return row[index]
}

fn count_char(str string, ch u8) int {
	mut count := 0
	for c in str {
		if c == ch {
			count++
		}
	}
	return count
}