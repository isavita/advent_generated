
import os
import strings

fn priority(ch u8) int {
	if ch >= u8(`a`) && ch <= u8(`z`) {
		return int(ch - u8(`a`)) + 1
	}
	return int(ch - u8(`A`)) + 27
}

fn main() {
	data := os.read_file('input.txt') or { panic(err) }
	lines := data.split('\n')
	mut sum := 0
	mut i := 0
	for i < lines.len {
		mut group := []string{cap: 3}
		for group.len < 3 && i < lines.len {
			line := lines[i].trim_space()
			if line.len > 0 {
				group << line
			}
			i++
		}
		if group.len < 3 { break }
		mut present := [256]bool{}
		for ch in group[0].bytes() {
			present[ch] = true
		}
		mut common := [256]bool{}
		for ch in group[1].bytes() {
			if present[ch] {
				common[ch] = true
			}
		}
		for ch in group[2].bytes() {
			if common[ch] {
				sum += priority(ch)
				break
			}
		}
	}
	println(sum)
}
