import os
import strconv

fn main() {
	mut counts := []u64{len: 9}
	data := os.read_file('input.txt') or { exit(1) }
	line := data.trim_space()
	for token in line.split(',') {
		n := token.int()
		counts[n]++
	}
	for _ in 0 .. 80 {
		new_fish := counts[0]
		for i in 1 .. 9 {
			counts[i - 1] = counts[i]
		}
		counts[6] += new_fish
		counts[8] = new_fish
	}
	mut total := u64(0)
	for v in counts {
		total += v
	}
	println(total)
}