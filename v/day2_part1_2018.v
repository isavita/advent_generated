import os

fn main() {
	mut two_count := 0
	mut three_count := 0
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		twos, threes := count_twos_and_threes(line)
		if twos { two_count++ }
		if threes { three_count++ }
	}

	checksum := two_count * three_count
	println(checksum)
}

fn count_twos_and_threes(id string) (bool, bool) {
	mut char_count := map[rune]int{}
	for c in id {
		char_count[c]++
	}

	mut has_twos := false
	mut has_threes := false
	for count in char_count.values() {
		if count == 2 { has_twos = true }
		if count == 3 { has_threes = true }
	}
	return has_twos, has_threes
}