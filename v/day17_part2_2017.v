import os

fn main() {
	data := os.read_file('input.txt') or { return }
	steps := data.trim_space().int()
	mut current_pos := 0
	mut value_after_zero := 0

	for i in 1 .. 50000001 {
		current_pos = (current_pos + steps) % i
		if current_pos == 0 {
			value_after_zero = i
		}
		current_pos++
	}

	println(value_after_zero)
}