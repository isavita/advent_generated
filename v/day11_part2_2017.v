import os

fn abs(x int) int {
	return if x < 0 { -x } else { x }
}

fn max(a int, b int) int {
	return if a > b { a } else { b }
}

fn distance(x int, y int, z int) int {
	return (abs(x) + abs(y) + abs(z)) / 2
}

fn main() {
	input := os.read_file('input.txt') or { return }
	directions := input.split(',').map(it.trim_space())

	mut x := 0
	mut y := 0
	mut z := 0
	mut max_distance := 0

	for dir in directions {
		match dir {
			'n' { y++ z-- }
			'ne' { x++ z-- }
			'se' { x++ y-- }
			's' { y-- z++ }
			'sw' { x-- z++ }
			'nw' { x-- y++ }
			else {}
		}
		cur_distance := distance(x, y, z)
		max_distance = max(max_distance, cur_distance)
	}

	println(max_distance)
}