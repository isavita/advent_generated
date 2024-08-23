import os

fn main() {
	mut sum := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		first_digit := find_digit(line, true)
		last_digit := find_digit(line, false)
		if first_digit != -1 && last_digit != -1 {
			sum += first_digit * 10 + last_digit
		}
	}

	println(sum)
}

fn find_digit(s string, first bool) int {
	digits := ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
	words := ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']

	for i in 0 .. s.len {
		idx := if first { i } else { s.len - 1 - i }
		c := s[idx].ascii_str()
		for j, digit in digits {
			if c == digit || s[idx..].starts_with(words[j]) {
				return j
			}
		}
	}
	return -1
}