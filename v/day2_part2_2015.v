import os

fn main() {
	mut total_ribbon := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		dimensions := line.split('x').map(it.int())
		if dimensions.len != 3 { return }

		l, w, h := dimensions[0], dimensions[1], dimensions[2]
		bow := l * w * h
		mut sides := [l, w, h]
		sides.sort()

		wrap := 2 * sides[0] + 2 * sides[1]

		total_ribbon += bow + wrap
	}

	println(total_ribbon)
}