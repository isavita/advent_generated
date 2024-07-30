import os

struct Point {
	x int
	y int
}

fn main() {
	mut visited := map[string]bool{}
	mut santa_pos := Point{0, 0}
	mut robo_pos := Point{0, 0}
	input := os.read_file('input.txt') or { return }

	for i, direction in input.trim_space().bytes() {
		if i % 2 == 0 {
			santa_pos = move_santa(santa_pos, direction)
			visited[point_key(santa_pos)] = true
		} else {
			robo_pos = move_santa(robo_pos, direction)
			visited[point_key(robo_pos)] = true
		}
	}

	println(visited.len)
}

fn move_santa(pos Point, direction byte) Point {
	match direction {
		`^` { return Point{pos.x, pos.y + 1} }
		`v` { return Point{pos.x, pos.y - 1} }
		`>` { return Point{pos.x + 1, pos.y} }
		`<` { return Point{pos.x - 1, pos.y} }
		else { return pos }
	}
}

fn point_key(p Point) string {
	return '${p.x},${p.y}'
}