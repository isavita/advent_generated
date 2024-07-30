import os

fn main() {
	mfcsam := {
		'children': 3, 'cats': 7, 'samoyeds': 2, 'pomeranians': 3,
		'akitas': 0, 'vizslas': 0, 'goldfish': 5, 'trees': 3,
		'cars': 2, 'perfumes': 1,
	}

	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		parts := line.split(' ')
		sue_number := parts[1][..parts[1].len - 1]

		mut matches := true
		for i in 2 .. parts.len {
			if i % 2 == 0 {
				item := parts[i][..parts[i].len - 1]
				count := parts[i + 1][..parts[i + 1].len - 1].int()
				if mfcsam[item] != count {
					matches = false
					break
				}
			}
		}

		if matches {
			println(sue_number)
			break
		}
	}
}