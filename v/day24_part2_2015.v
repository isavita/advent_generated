import os
import strconv

fn can_split(packages []int, first_comb int, target int) bool {
	mut remaining := []int{}
	for i, v in packages {
		if first_comb & (1 << i) == 0 {
			remaining << v
		}
	}
	n := remaining.len
	mut comb1 := 1
	for comb1 < (1 << n) {
		mut w1 := 0
		for i in 0 .. n {
			if comb1 & (1 << i) != 0 {
				w1 += remaining[i]
			}
		}
		if w1 == target {
			mut comb2 := 1
			for comb2 < (1 << n) {
				if comb1 & comb2 == 0 {
					mut w2 := 0
					for i in 0 .. n {
						if comb2 & (1 << i) != 0 {
							w2 += remaining[i]
						}
					}
					if w2 == target {
						return true
					}
				}
				comb2++
			}
		}
		comb1++
	}
	return false
}

fn main() {
	content := os.read_file('input.txt') or { return }
	lines := content.split('\n')
	mut packages := []int{}
	mut total := 0
	for line in lines {
		trim := line.trim_space()
		if trim.len == 0 { continue }
		v := trim.int()
		packages << v
		total += v
	}
	parts := 4
	target := total / parts
	n := packages.len
	mut best_qe := i64(9223372036854775807)
	mut best_len := int(2147483647)

	mut comb := 1
	for comb < (1 << n) {
		mut w := 0
		mut qe := i64(1)
		mut l := 0
		for i in 0 .. n {
			if comb & (1 << i) != 0 {
				w += packages[i]
				qe *= i64(packages[i])
				l++
			}
		}
		if w == target && l <= best_len {
			if l < best_len || qe < best_qe {
				if can_split(packages, comb, target) {
					best_len = l
					best_qe = qe
				}
			}
		}
		comb++
	}
	println(best_qe)
}