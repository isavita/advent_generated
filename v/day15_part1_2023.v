
import os

fn hash(s string) int {
	mut current_value := 0
	for c in s {
		current_value += int(c)
		current_value *= 17
		current_value %= 256
	}
	return current_value
}

fn main() {
	if !os.exists('input.txt') {
		println('Error: input.txt not found')
		return
	}
	input := os.read_file('input.txt') or {
		println('Error reading input.txt: $err')
		return
	}
	
	steps := input.trim_space().split(',')
	mut sum := 0
	for step in steps {
		sum += hash(step)
	}
	println(sum)
}
