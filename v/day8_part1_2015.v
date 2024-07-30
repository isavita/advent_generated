import os

fn main() {
	mut total_diff := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		code_length := line.len
		memory_length := calculate_memory_length(line)
		total_diff += code_length - memory_length
	}

	println(total_diff)
}

fn calculate_memory_length(s string) int {
	mut length := 0
	mut in_escape := false
	mut hex_count := 0

	for i in 1 .. s.len - 1 {
		if hex_count > 0 {
			hex_count--
		} else if in_escape {
			if s[i] == `x` {
				hex_count = 2
			}
			in_escape = false
			length++
		} else if s[i] == `\\` { // Changed backtick to double quotes
			in_escape = true
		} else {
			length++
		}
	}
	return length
}