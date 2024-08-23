import os

const grid_size = 1000

fn main() {
	mut lights := [][]bool{len: grid_size, init: []bool{len: grid_size}}
	input := os.read_file('input.txt') or { panic(err) }

	for line in input.split('\n') {
		if line.trim_space() == '' {
			continue
		}
		process_instruction(line, mut lights)
	}

	count := count_lit_lights(lights)
	println(count)
}

fn process_instruction(instruction string, mut lights [][]bool) {
	parts := instruction.split(' ')
	mut action := ''
	mut start := [0, 0]
	mut end := [0, 0]

	if parts[0] == 'toggle' {
		action = 'toggle'
		start = parse_coords(parts[1])
		end = parse_coords(parts[3])
	} else {
		action = parts[1]
		start = parse_coords(parts[2])
		end = parse_coords(parts[4])
	}

	for i in start[0]..end[0] + 1 {
		for j in start[1]..end[1] + 1 {
			match action {
				'toggle' { lights[i][j] = !lights[i][j] }
				'on' { lights[i][j] = true }
				'off' { lights[i][j] = false }
				else {}
			}
		}
	}
}

fn parse_coords(coord_str string) []int {
	coords := coord_str.split(',')
	return [coords[0].int(), coords[1].int()]
}

fn count_lit_lights(lights [][]bool) int {
	mut count := 0
	for row in lights {
		for light in row {
			if light {
				count++
			}
		}
	}
	return count
}