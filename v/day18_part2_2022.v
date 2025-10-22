
import os

struct Coord {
	x int
	y int
	z int
}

const neighbors = [
	Coord{-1, 0, 0}, Coord{1, 0, 0},
	Coord{0, -1, 0}, Coord{0, 1, 0},
	Coord{0, 0, -1}, Coord{0, 0, 1},
]

fn main() {
	mut cubes := []Coord{cap: 3000}
	mut min_x, mut min_y, mut min_z := int(0x7FFFFFFF), int(0x7FFFFFFF), int(0x7FFFFFFF)
	mut max_x, mut max_y, mut max_z := int(-0x7FFFFFFF-1), int(-0x7FFFFFFF-1), int(-0x7FFFFFFF-1)

	content := os.read_file('input.txt') or { panic(err) }
	for line in content.split_into_lines() {
		if line.len == 0 { continue }
		parts := line.split(',')
		if parts.len != 3 { continue }
		cube := Coord{parts[0].int(), parts[1].int(), parts[2].int()}
		cubes << cube
		if cube.x < min_x { min_x = cube.x }
		if cube.y < min_y { min_y = cube.y }
		if cube.z < min_z { min_z = cube.z }
		if cube.x > max_x { max_x = cube.x }
		if cube.y > max_y { max_y = cube.y }
		if cube.z > max_z { max_z = cube.z }
	}

	min_x--; min_y--; min_z--
	max_x++; max_y++; max_z++

	dx := max_x - min_x + 1
	dy := max_y - min_y + 1
	dz := max_z - min_z + 1

	mut grid := [][][]u8{len: dx, init: [][]u8{len: dy, init: []u8{len: dz, init: 0}}}
	for c in cubes {
		grid[c.x - min_x][c.y - min_y][c.z - min_z] = 1
	}

	mut faces := 0
	mut q := []Coord{cap: 10000}
	q << Coord{0, 0, 0}
	grid[0][0][0] = 2

	for q.len > 0 {
		curr := q.pop()
		for d in neighbors {
			nx := curr.x + d.x
			ny := curr.y + d.y
			nz := curr.z + d.z
			if nx < 0 || nx >= dx || ny < 0 || ny >= dy || nz < 0 || nz >= dz { continue }
			match grid[nx][ny][nz] {
				1 { faces++ }
				0 {
					grid[nx][ny][nz] = 2
					q << Coord{nx, ny, nz}
				}
				else {}
			}
		}
	}
	println(faces)
}
