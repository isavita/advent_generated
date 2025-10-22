
import os

const (
	screen_width  = 50
	screen_height = 6
)

fn main() {
	raw := os.read_file('input.txt') or { return }
	mut screen := [][]bool{len: screen_height, init: []bool{len: screen_width}}
	for line in raw.split_into_lines() {
		process(line, mut screen)
	}
	println(count(screen))
}

fn process(inst string, mut scr [][]bool) {
	if inst.starts_with('rect ') {
		parts := inst[5..].split('x')
		a := parts[0].int()
		b := parts[1].int()
		for y in 0 .. b {
			for x in 0 .. a {
				scr[y][x] = true
			}
		}
	} else if inst.starts_with('rotate row y=') {
		parts := inst[13..].split(' by ')
		row := parts[0].int()
		shift := parts[1].int()
		mut tmp := []bool{len: screen_width}
		for i, v in scr[row] {
			tmp[(i + shift) % screen_width] = v
		}
		scr[row] = tmp
	} else if inst.starts_with('rotate column x=') {
		parts := inst[16..].split(' by ')
		col := parts[0].int()
		shift := parts[1].int()
		mut tmp := []bool{len: screen_height}
		for i in 0 .. screen_height {
			tmp[(i + shift) % screen_height] = scr[i][col]
		}
		for i in 0 .. screen_height {
			scr[i][col] = tmp[i]
		}
	}
}

fn count(scr [][]bool) int {
	mut n := 0
	for row in scr {
		for p in row {
			if p {
				n++
			}
		}
	}
	return n
}
