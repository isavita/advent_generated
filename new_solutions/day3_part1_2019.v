import os

struct Point {
	x int
	y int
}

fn main() {
	mut wires := [][]string{}
	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		wires << line.split(',')
	}

	wire1 := trace_wire(wires[0])
	wire2 := trace_wire(wires[1])

	mut closest_distance := int(1e9)
	for point in wire1 {
		if point in wire2 {
			distance := manhattan_distance(point)
			if distance < closest_distance {
				closest_distance = distance
			}
		}
	}

	println(closest_distance)
}

fn trace_wire(path []string) []Point {
	mut points := []Point{}
	mut x := 0
	mut y := 0

	for move in path {
		direction := move[0]
		length := move[1..].int()
		for _ in 0 .. length {
			match direction {
				`R` { x++ }
				`L` { x-- }
				`U` { y++ }
				`D` { y-- }
				else {}
			}
			points << Point{x, y}
		}
	}
	return points
}

fn manhattan_distance(p Point) int {
	return abs(p.x) + abs(p.y)
}

fn abs(n int) int {
	return if n < 0 { -n } else { n }
}