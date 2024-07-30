import os

const preamble_length = 25

fn main() {
	mut numbers := []int{}
	lines := os.read_lines('input.txt') or { panic('Error reading file') }

	for line in lines {
		numbers << line.int()
	}

	for i in preamble_length .. numbers.len {
		if !is_valid(numbers[i], numbers[i - preamble_length .. i]) {
			println(numbers[i])
			break
		}
	}
}

fn is_valid(number int, previous_numbers []int) bool {
	mut seen := map[int]bool{}
	for n in previous_numbers {
		if seen[number - n] {
			return true
		}
		seen[n] = true
	}
	return false
}