
import os

fn main() {
	mut weights := []int{}
	mut total := 0
	data := os.read_file('input.txt') or { panic('cannot read input.txt') }
	for line in data.split('\n') {
		if line.trim_space() == '' {
			continue
		}
		w := line.int()
		weights << w
		total += w
	}
	n := weights.len
	target := total / 3
	mut best_qe := u64(-1) // max value
	mut best_len := int(-1) // max int
	mut max_mask := 1 << n
	for mask in 1 .. max_mask {
		mut sum := 0
		mut qe := u64(1)
		mut len := 0
		for i in 0 .. n {
			if mask & (1 << i) != 0 {
				sum += weights[i]
				if sum > target {
					break
				}
				qe *= u64(weights[i])
				len += 1
			}
		}
		if sum == target {
			if best_len == -1 || len < best_len || (len == best_len && qe < best_qe) {
				best_len = len
				best_qe = qe
			}
		}
	}
	println(best_qe)
}
