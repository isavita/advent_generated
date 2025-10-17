
import os

const max_size = 10000

enum State { clean weakened infected flagged }

fn hash(x int, y int) int {
	return (x + max_size) * max_size + (y + max_size)
}

fn main() {
	raw := os.read_file('input.txt') or { panic(err) }
	lines := raw.split_into_lines()
	mut grid := map[int]State{}
	start_x := lines[0].len / 2
	start_y := lines.len / 2
	for y, line in lines {
		for x, ch in line {
			if ch == `#` {
				grid[hash(x, y)] = .infected
			}
		}
	}
	mut x, mut y, mut dir, mut infected := start_x, start_y, 0, 0
	dx := [0, 1, 0, -1]
	dy := [-1, 0, 1, 0]
	for _ in 0 .. 10_000_000 {
		idx := hash(x, y)
		match grid[idx] {
			.clean {
				dir = (dir + 3) % 4
				grid[idx] = .weakened
			}
			.weakened {
				grid[idx] = .infected
				infected++
			}
			.infected {
				dir = (dir + 1) % 4
				grid[idx] = .flagged
			}
			.flagged {
				dir = (dir + 2) % 4
				grid.delete(idx)
			}
		}
		x += dx[dir]
		y += dy[dir]
	}
	println(infected)
}
