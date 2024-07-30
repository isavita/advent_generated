import os

fn main() {
	mut nice := 0
	input := os.read_file('input.txt') or { panic(err) }.trim_space()

	passes_rule1 := fn (line string) bool {
		for i in 0 .. line.len - 2 {
			to_match := line[i .. i + 2]
			for j in (i + 2) .. line.len - 1 {
				if line[j .. j + 2] == to_match {
					return true
				}
			}
		}
		return false
	}

	for line in input.split('\n') {
		rule1 := passes_rule1(line)
		mut rule2 := false
		for i in 0 .. line.len - 2 {
			if line[i] == line[i + 2] {
				rule2 = true
				break
			}
		}
		if rule1 && rule2 {
			nice++
		}
	}

	println(nice)
}