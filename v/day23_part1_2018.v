import os
import math

struct Nanobot {
	pos []int
	radius int
}

fn main() {
	input := os.read_file('input.txt') or { panic(err) }
	mut nanobots := []Nanobot{}

	for line in input.split('\n') {
		if line.trim_space() == '' {
			continue
		}
		parts := line.split(', ')
		pos := parts[0][5..].split(',').map(it.int())
		radius := parts[1][2..].int()
		nanobots << Nanobot{pos, radius}
	}

	mut strongest := nanobots[0]
	for nb in nanobots {
		if nb.radius > strongest.radius {
			strongest = nb
		}
	}

	mut count := 0
	for nb in nanobots {
		distance := manhattan_distance(strongest.pos, nb.pos)
		if distance <= strongest.radius {
			count++
		}
	}

	println(count)
}

fn manhattan_distance(a []int, b []int) int {
	return math.abs(a[0] - b[0]) + math.abs(a[1] - b[1]) + math.abs(a[2] - b[2])
}