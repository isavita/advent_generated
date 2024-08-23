import os

struct Disc {
	pos int
	start int
}

fn main() {
	mut discs := []Disc{}
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split(' ')
		pos := parts[3].int()
		start := parts[11].replace('.', '').int()
		discs << Disc{pos, start}
	}

	discs << Disc{11, 0}

	mut time := 0
	for {
		mut all_aligned := true
		for i, disc in discs {
			if (time + i + 1 + disc.start) % disc.pos != 0 {
				all_aligned = false
				break
			}
		}
		if all_aligned {
			println(time)
			break
		}
		time++
	}
}