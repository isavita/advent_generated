import os

fn main() {
	mut data := os.read_file('input.txt') or { panic(err) }
	mut score := 0
	mut garbage_count := 0
	mut in_garbage := false
	mut ignore_next := false

	for i := 0; i < data.len; i++ {
		c := data[i]
		if ignore_next {
			ignore_next = false
		} else if c == `!` {
			ignore_next = true
		} else if in_garbage {
			if c == `>` {
				in_garbage = false
			} else {
				garbage_count++
			}
		} else if c == `<` {
			in_garbage = true
		} else if c == `{` {
			score++
		}
	}
	println(score)
	println(garbage_count)
}