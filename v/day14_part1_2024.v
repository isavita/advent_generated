
import os

struct Robot {
	mut:
		x int
		y int
		vx int
		vy int
}

fn main() {
	lines := os.read_lines('input.txt') or { panic(err) }

	mut robots := []Robot{}
	for line in lines {
		parts := line.split(' ')
		pos := parts[0][2..].split(',')
		vel := parts[1][2..].split(',')
		robots << Robot{
			x: pos[0].int()
			y: pos[1].int()
			vx: vel[0].int()
			vy: vel[1].int()
		}
	}

	width := 101
	height := 103

	for _ in 0 .. 100 {
		for mut robot in robots {
			robot.x += robot.vx
			robot.y += robot.vy

			robot.x = (robot.x % width + width) % width
			robot.y = (robot.y % height + height) % height
		}
	}

	mut grid := [][]int{len: height, init: []int{len: width}}
	for robot in robots {
		grid[robot.y][robot.x]++
	}

	mid_x := width / 2
	mid_y := height / 2

	mut q1 := 0
	mut q2 := 0
	mut q3 := 0
	mut q4 := 0

	for y in 0 .. height {
		for x in 0 .. width {
			if x < mid_x && y < mid_y {
				q1 += grid[y][x]
			} else if x > mid_x && y < mid_y {
				q2 += grid[y][x]
			} else if x < mid_x && y > mid_y {
				q3 += grid[y][x]
			} else if x > mid_x && y > mid_y {
				q4 += grid[y][x]
			}
		}
	}

	println(q1 * q2 * q3 * q4)
}
