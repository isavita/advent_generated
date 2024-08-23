import os

fn main() {
	mut valid_count := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split(' ')
		range_parts := parts[0].split('-')
		min_count := range_parts[0].int()
		max_count := range_parts[1].int()
		letter := parts[1][0]
		password := parts[2]

		count := password.count(letter.ascii_str())
		if count >= min_count && count <= max_count {
			valid_count++
		}
	}
	println(valid_count)
}