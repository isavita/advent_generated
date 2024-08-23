import os

fn main() {
	mut head := [0, 0]
	mut tail := [0, 0]
	mut visited := map[string]bool{}

	lines := os.read_lines('input.txt') or { panic(err) }

	for line in lines {
		parts := line.split(' ')
		direction := parts[0]
		steps := parts[1].int()

		for _ in 0 .. steps {
			match direction {
				'R' { head[0]++ }
				'L' { head[0]-- }
				'U' { head[1]++ }
				'D' { head[1]-- }
				else {}
			}

			if abs(head[0] - tail[0]) > 1 || abs(head[1] - tail[1]) > 1 {
				if head[0] > tail[0] { tail[0]++ }
				if head[0] < tail[0] { tail[0]-- }
				if head[1] > tail[1] { tail[1]++ }
				if head[1] < tail[1] { tail[1]-- }
			}

			visited['${tail[0]},${tail[1]}'] = true
		}
	}

	println(visited.len)
}

fn abs(x int) int {
	return if x < 0 { -x } else { x }
}