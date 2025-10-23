
import os
import strconv

struct Robot {
mut:
	x int
	y int
	vx int
	vy int
}

fn parse_robots(input string) []Robot {
	mut robots := []Robot{}
	for line in input.trim_space().split_into_lines() {
		if line.len == 0 { continue }
		parts := line.split(' ')
		p := parts[0][2..].split(',')
		v := parts[1][2..].split(',')
		robots << Robot{
			x: strconv.atoi(p[0]) or { 0 }
			y: strconv.atoi(p[1]) or { 0 }
			vx: strconv.atoi(v[0]) or { 0 }
			vy: strconv.atoi(v[1]) or { 0 }
		}
	}
	return robots
}

fn simulate(mut robots []Robot, width int, height int, steps int) {
	for step in 0 .. steps {
		for mut robot in robots {
			robot.x = (robot.x + robot.vx) % width
			if robot.x < 0 { robot.x += width }
			robot.y = (robot.y + robot.vy) % height
			if robot.y < 0 { robot.y += height }
		}
	}
}

fn count_in_quadrants(robots []Robot, width int, height int) int {
	mid_x := width / 2
	mid_y := height / 2
	
	mut q1 := 0
	mut q2 := 0
	mut q3 := 0
	mut q4 := 0
	
	for robot in robots {
		if robot.x == mid_x || robot.y == mid_y { continue }
		
		if robot.x < mid_x && robot.y < mid_y {
			q1++
		} else if robot.x > mid_x && robot.y < mid_y {
			q2++
		} else if robot.x < mid_x && robot.y > mid_y {
			q3++
		} else if robot.x > mid_x && robot.y > mid_y {
			q4++
		}
	}
	
	return q1 * q2 * q3 * q4
}

fn is_tree_pattern(robots []Robot, width int, height int) bool {
	// Check if robots form a contiguous vertical line (simple tree indicator)
	mut positions := map[string]bool{}
	for robot in robots {
		positions['${robot.x},${robot.y}'] = true
	}
	
	// Look for a vertical line of at least 10 robots
	for x in 0 .. width {
		mut consecutive := 0
		for y in 0 .. height {
			if positions['${x},${y}'] {
				consecutive++
				if consecutive >= 10 { return true }
			} else {
				consecutive = 0
			}
		}
	}
	return false
}

fn main() {
	input := os.read_file('input.txt') or { panic('Cannot read input.txt') }
	mut robots := parse_robots(input)
	
	// Part 1
	mut robots_copy := robots.clone()
	simulate(mut robots_copy, 101, 103, 100)
	safety_factor := count_in_quadrants(robots_copy, 101, 103)
	println('Part 1: ${safety_factor}')
	
	// Part 2 - Find the tree pattern
	mut seconds := 0
	for {
		simulate(mut robots, 101, 103, 1)
		seconds++
		if is_tree_pattern(robots, 101, 103) {
			println('Part 2: ${seconds}')
			break
		}
	}
}
