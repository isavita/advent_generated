import os

fn main() {
	data := os.read_file("input.txt") or { return }
	steps := data.trim_space().int()
	mut buffer := []int{cap: 1}
	buffer << 0
	mut current_pos := 0

	for i in 1 .. 2018 {
		current_pos = (current_pos + steps) % buffer.len
		buffer.insert(current_pos + 1, i)
		current_pos++
	}

	for i in 0 .. buffer.len {
		if buffer[i] == 2017 {
			println(buffer[(i + 1) % buffer.len])
			break
		}
	}
}