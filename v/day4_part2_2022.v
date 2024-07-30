import os

fn main() {
	mut count := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		pair := line.split(',')
		left := parse_range(pair[0])
		right := parse_range(pair[1])

		if left[0] <= right[1] && left[1] >= right[0] {
			count++
		}
	}

	println(count)
}

fn parse_range(s string) []int {
	parts := s.split('-')
	return [parts[0].int(), parts[1].int()]
}