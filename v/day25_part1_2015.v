
import os

fn main() {
	lines := os.read_lines('input.txt') or { panic(err) }
	row, col := parse_input(lines[0])
	code := find_code(row, col)
	println(code)
}

fn parse_input(line string) (int, int) {
	mut row := 0
	mut col := 0
	parts := line.split(' ')
	for i, part in parts {
		if part == 'row' {
			row = parts[i + 1].replace(',', '').int()
		} else if part == 'column' {
			col = parts[i + 1].replace('.', '').int()
		}
	}
	return row, col
}

fn find_code(row int, col int) i64 {
	index := calculate_index(row, col)
	mut code := i64(20151125)
	for _ in 1 .. index {
		code = (code * 252533) % 33554393
	}
	return code
}

fn calculate_index(row int, col int) int {
	diagonal := row + col - 1
	index := (diagonal * (diagonal - 1)) / 2 + col
	return index
}
