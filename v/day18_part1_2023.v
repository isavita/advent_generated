module main

import os

struct Coord {
	x int
	y int
}

fn (c1 Coord) add(c2 Coord) Coord {
	return Coord{c1.x + c2.x, c1.y + c2.y}
}

fn (c Coord) multiply_by_scalar(s int) Coord {
	return Coord{c.x * s, c.y * s}
}

const (
	north = Coord{0, -1}
	west  = Coord{-1, 0}
	south = Coord{0, 1}
	east  = Coord{1, 0}
)

fn abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

fn parse_input(input []string) []Coord {
	mut current := Coord{0, 0}
	mut vertices := [current]

	for line in input {
		parts := line.split(' ')
		dir_input := parts[0][0]
		length := parts[1].int()

		mut dir := Coord{}
		match dir_input {
			`U` { dir = north }
			`L` { dir = west }
			`D` { dir = south }
			`R` { dir = east }
			else { continue } // Handle invalid direction inputs by skipping
		}

		current = current.add(dir.multiply_by_scalar(length))
		vertices << current
	}

	return vertices
}

fn shoelace(vertices []Coord) int {
	mut area := 0
	n := vertices.len

	for i in 0 .. n {
		next := (i + 1) % n
		area += vertices[i].x * vertices[next].y - vertices[i].y * vertices[next].x
	}

	return abs(area) / 2
}

fn perimeter(vertices []Coord) int {
	mut perim := 0
	n := vertices.len

	for i in 0 .. n {
		next := (i + 1) % n
		perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
	}

	return perim
}

fn calculate_polygon_area(vertices []Coord) int {
	return shoelace(vertices) + perimeter(vertices) / 2 + 1
}

fn solve(input []string) int {
	vertices := parse_input(input)
	return calculate_polygon_area(vertices)
}

fn main() {
	input := os.read_lines('input.txt') or { panic(err) }
	result := solve(input)
	println(result)
}