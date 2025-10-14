import os

fn main() {
	data := os.read_file('input.txt') or { return }
	mut sum := 0
	mut num := 0
	mut is_negative := false
	for c in data {
		if c == `-` {
			is_negative = true
		} else if c >= `0` && c <= `9` {
			num = num * 10 + int(c - `0`)
		} else {
			if is_negative { num = -num }
			sum += num
			num = 0
			is_negative = false
		}
	}
	if is_negative { num = -num }
	sum += num
	println(sum)
}