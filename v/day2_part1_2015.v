import os

fn main() {
	mut total := 0
	lines := os.read_lines('input.txt') or { panic(err) }
	for line in lines {
		dimensions := line.split('x').map(it.int())
		if dimensions.len != 3 { panic('Invalid input format') }
		l, w, h := dimensions[0], dimensions[1], dimensions[2]
		side1 := l * w
		side2 := w * h
		side3 := h * l
		smallest := min(side1, side2, side3)
		total += 2 * side1 + 2 * side2 + 2 * side3 + smallest
	}
	println(total)
}

fn min(a int, b int, c int) int {
	return if a < b && a < c { a } else if b < c { b } else { c }
}