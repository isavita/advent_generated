module main

import os
import math

fn main() {
	asteroids := read_asteroids('input.txt')
	max_count := find_best_asteroid_location(asteroids)
	println(max_count)
}

fn read_asteroids(filename string) [][]bool {
	mut asteroids := [][]bool{}
	lines := os.read_lines(filename) or { panic(err) }
	for line in lines {
		mut asteroid_row := []bool{}
		for c in line {
			asteroid_row << (c == `#`)
		}
		asteroids << asteroid_row
	}
	return asteroids
}

fn find_best_asteroid_location(asteroids [][]bool) int {
	mut max_count := 0
	for y, row in asteroids {
		for x, is_asteroid in row {
			if is_asteroid {
				count := count_visible_asteroids(asteroids, x, y)
				if count > max_count {
					max_count = count
				}
			}
		}
	}
	return max_count
}

fn count_visible_asteroids(asteroids [][]bool, x int, y int) int {
	mut angles := map[f64]bool{}
	for other_y, row in asteroids {
		for other_x, is_asteroid in row {
			if is_asteroid && !(other_x == x && other_y == y) {
				angle := math.atan2(f64(other_y - y), f64(other_x - x))
				angles[angle] = true
			}
		}
	}
	return angles.len
}