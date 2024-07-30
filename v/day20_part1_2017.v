import os

struct Particle {
mut:
	p [3]int
	v [3]int
	a [3]int
}

fn abs(x int) int {
	return if x < 0 { -x } else { x }
}

fn manhattan(x [3]int) int {
	return abs(x[0]) + abs(x[1]) + abs(x[2])
}

fn main() {
	mut particles := []Particle{}
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split(', ')
		mut p := Particle{}
		for i, part in parts {
			coords := part[3..part.len - 1].split(',')
			for j, coord in coords {
				match i {
					0 { p.p[j] = coord.int() }
					1 { p.v[j] = coord.int() }
					2 { p.a[j] = coord.int() }
					else {}
				}
			}
		}
		particles << p
	}

	mut closest_particle := 0
	mut min_accel := 1 << 31 - 1
	mut min_velocity := 1 << 31 - 1
	mut min_position := 1 << 31 - 1

	for i, particle in particles {
		accel := manhattan(particle.a)
		velocity := manhattan(particle.v)
		position := manhattan(particle.p)

		if accel < min_accel || (accel == min_accel && velocity < min_velocity) ||
			(accel == min_accel && velocity == min_velocity && position < min_position) {
			min_accel = accel
			min_velocity = velocity
			min_position = position
			closest_particle = i
		}
	}

	println(closest_particle)
}