
import os

struct Position {
	x int
	y int
}

fn main() {
	raw := os.read_file('input.txt') or { panic('missing input.txt') }
	lines := raw.trim('\n').split_into_lines()
	mut infected := map[int]bool{}
	mut y := 0
	for row in lines {
		for x, c in row {
			if c == `#` {
				infected[1024 * y + x] = true
			}
		}
		y++
	}
	start_x := lines[0].len / 2
	start_y := y / 2

	dx := [0, 1, 0, -1]
	dy := [-1, 0, 1, 0]
	mut dir := 0
	mut x := start_x
	mut cy := start_y
	mut new_infections := 0

	for _ in 0 .. 10000 {
		key := 1024 * cy + x
		if infected[key] {
			dir = (dir + 1) % 4
			infected.delete(key)
		} else {
			dir = (dir + 3) % 4
			infected[key] = true
			new_infections++
		}
		x += dx[dir]
		cy += dy[dir]
	}
	println(new_infections)
}
