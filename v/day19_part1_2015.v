import os

fn main() {
	mut replacements := []string{}
	mut molecule := ""

	lines := os.read_lines("input.txt") or { return }
	for line in lines {
		if line.len == 0 {
			continue
		}
		if line.contains(' => ') {
			replacements << line
		} else {
			molecule = line
		}
	}

	mut molecules := map[string]bool{}
	for replacement in replacements {
		parts := replacement.split(' => ')
		for i in 0 .. molecule.len {
			if molecule[i..].starts_with(parts[0]) {
				new_molecule := molecule[..i] + parts[1] + molecule[i + parts[0].len..]
				molecules[new_molecule] = true
			}
		}
	}

	println(molecules.len)
}