import os

struct Cuboid {
	x1 int
	x2 int
	y1 int
	y2 int
	z1 int
	z2 int
	on bool
}

fn main() {
	mut cuboids := []Cuboid{}
	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		parts := line.split(' ')
		on := parts[0] == 'on'
		ranges := parts[1].split(',')
		x_range := ranges[0][2..].split('..')
		y_range := ranges[1][2..].split('..')
		z_range := ranges[2][2..].split('..')
		cuboids << Cuboid{
			x1: x_range[0].int()
			x2: x_range[1].int()
			y1: y_range[0].int()
			y2: y_range[1].int()
			z1: z_range[0].int()
			z2: z_range[1].int()
			on: on
		}
	}

	mut grid := map[string]bool{}
	for cuboid in cuboids {
		for x in max(cuboid.x1, -50)..min(cuboid.x2, 50) + 1 {
			for y in max(cuboid.y1, -50)..min(cuboid.y2, 50) + 1 {
				for z in max(cuboid.z1, -50)..min(cuboid.z2, 50) + 1 {
					key := '$x,$y,$z'
					if cuboid.on {
						grid[key] = true
					} else {
						grid.delete(key)
					}
				}
			}
		}
	}
	println(grid.len)
}

fn max(a int, b int) int {
	return if a > b { a } else { b }
}

fn min(a int, b int) int {
	return if a < b { a } else { b }
}