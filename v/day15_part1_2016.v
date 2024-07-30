import os

struct Disc {
	pos int
	start int
}

fn read_input(file string) []Disc {
	mut discs := []Disc{}
	lines := os.read_lines(file) or { panic(err) }
	for line in lines {
		parts := line.split(' ')
		pos := parts[3].int()
		start := parts[11].trim_right('.').int()
		discs << Disc{pos, start}
	}
	return discs
}

fn can_pass(discs []Disc, time int) bool {
	for i, disc in discs {
		if (time + i + 1 + disc.start) % disc.pos != 0 {
			return false
		}
	}
	return true
}

fn find_first_time(discs []Disc) int {
	mut time := 0
	for !can_pass(discs, time) {
		time++
	}
	return time
}

fn main() {
	discs := read_input('input.txt')
	result := find_first_time(discs)
	println(result)
}