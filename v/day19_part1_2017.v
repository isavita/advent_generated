import os

struct Point {
mut:
	x int
	y int
}

fn main() {
	mut input := os.read_file('input.txt') or { panic(err) }
	lines := input.split('\n')
	mut pos := Point{0, 0}
	mut dir := Point{0, 1} // Start by going down
	mut letters := []string{}

	// Find starting position
	for x in 0 .. lines[0].len {
		if lines[0][x] == `|` || lines[0][x] == `-` || lines[0][x] == `+` {
			pos.x = x
			break
		}
	}

	for {
		if pos.y < 0 || pos.y >= lines.len || pos.x < 0 || pos.x >= lines[0].len {
			break
		}
		current := lines[pos.y][pos.x]

		if current == ` ` {
			break
		} else if current.is_letter() {
			letters << current.ascii_str()
		}

		if current == `+` {
			// Change direction
			if dir.y != 0 {
				if pos.x > 0 && lines[pos.y][pos.x - 1] != ` ` {
					dir = Point{-1, 0} // Left
				} else {
					dir = Point{1, 0} // Right
				}
			} else {
				if pos.y > 0 && lines[pos.y - 1][pos.x] != ` ` {
					dir = Point{0, -1} // Up
				} else {
					dir = Point{0, 1} // Down
				}
			}
		}

		pos.x += dir.x
		pos.y += dir.y
	}

	println(letters.join(''))
}