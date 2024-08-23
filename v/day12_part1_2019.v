module main

import os

struct Vector {
mut:
	x int
	y int
	z int
}

struct Moon {
mut:
	position Vector
	velocity Vector
}

fn main() {
	mut moons := []Moon{}
	lines := os.read_lines('input.txt') or { panic(err) }
	for line in lines {
		parts := line.split(',')
		x := parts[0][3..].int()
		y := parts[1][3..].int()
		z := parts[2][3..parts[2].len - 1].int()
		moons << Moon{position: Vector{x, y, z}, velocity: Vector{0, 0, 0}}
	}

	for _ in 0 .. 1000 {
		apply_gravity(mut moons)
		apply_velocity(mut moons)
	}

	total_energy := calculate_total_energy(moons)
	println(total_energy)
}

fn apply_gravity(mut moons []Moon) {
	for i in 0 .. moons.len {
		for j in 0 .. moons.len {
			if i != j {
				moons[i].velocity.x += compare(moons[j].position.x, moons[i].position.x)
				moons[i].velocity.y += compare(moons[j].position.y, moons[i].position.y)
				moons[i].velocity.z += compare(moons[j].position.z, moons[i].position.z)
			}
		}
	}
}

fn compare(a int, b int) int {
	return if a > b { 1 } else if a < b { -1 } else { 0 }
}

fn apply_velocity(mut moons []Moon) {
	for mut moon in moons {
		moon.position.x += moon.velocity.x
		moon.position.y += moon.velocity.y
		moon.position.z += moon.velocity.z
	}
}

fn calculate_total_energy(moons []Moon) int {
	mut total_energy := 0
	for moon in moons {
		potential_energy := abs(moon.position.x) + abs(moon.position.y) + abs(moon.position.z)
		kinetic_energy := abs(moon.velocity.x) + abs(moon.velocity.y) + abs(moon.velocity.z)
		total_energy += potential_energy * kinetic_energy
	}
	return total_energy
}

fn abs(value int) int {
	return if value < 0 { -value } else { value }
}