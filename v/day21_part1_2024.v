
import os

fn find_position(pad []string, ch u8) (int, int) {
	for i, row in pad {
		for j, c in row {
			if c == ch {
				return i, j
			}
		}
	}
	return -1, -1
}

fn ok(pad []string, i int, j int, seq string) bool {
	mut ci, mut cj := i, j
	for ch in seq {
		if pad[ci][cj] == ` ` {
			return false
		}
		match ch {
			`^` { ci-- }
			`v` { ci++ }
			`<` { cj-- }
			`>` { cj++ }
			else {}
		}
		if ci < 0 || ci >= pad.len || cj < 0 || cj >= pad[0].len {
			return false
		}
	}
	return true
}

fn generate_moves(pad []string, pi int, pj int, obj u8) string {
	oi, oj := find_position(pad, obj)
	mut h := ''
	if pj > oj {
		h += '<'.repeat(pj - oj)
	}
	if pi > oi {
		h += '^'.repeat(pi - oi)
	}
	if pi < oi {
		h += 'v'.repeat(oi - pi)
	}
	if pj < oj {
		h += '>'.repeat(oj - pj)
	}
	if ok(pad, pi, pj, h) {
		return h
	}
	h = ''
	if pj < oj {
		h += '>'.repeat(oj - pj)
	}
	if pi > oi {
		h += '^'.repeat(pi - oi)
	}
	if pi < oi {
		h += 'v'.repeat(oi - pi)
	}
	if pj > oj {
		h += '<'.repeat(pj - oj)
	}
	return h
}

struct Cache {
mut:
	m map[string]int
}

fn (mut c Cache) solve(code string, robots int, max int, keypad []string, robotpad []string) int {
	if robots == 0 {
		return code.len
	}
	k := '${code}|${robots}'
	if v := c.m[k] {
		return v
	}
	mut tot := 0
	mut pi, mut pj := 3, 2
	if robots != max {
		pi, pj = 0, 2
	}
	mut moves := ''
	for ch in code {
		if robots == max {
			moves = generate_moves(keypad, pi, pj, ch)
			pi, pj = find_position(keypad, ch)
		} else {
			moves = generate_moves(robotpad, pi, pj, ch)
			pi, pj = find_position(robotpad, ch)
		}
		tot += c.solve(moves + 'A', robots - 1, max, keypad, robotpad)
	}
	c.m[k] = tot
	return tot
}

fn main() {
	keypad := ['789', '456', '123', ' 0A']
	robotpad := [' ^A', '<v>']
	mut sum := 0
	content := os.read_file('input.txt') or { panic(err) }
	for line in content.split_into_lines() {
		if line == '' { continue }
		mut num := 0
		for ch in line {
			if ch >= `0` && ch <= `9` {
				num = num * 10 + int(ch - `0`)
			}
		}
		mut c := Cache{}
		sum += c.solve(line, 3, 3, keypad, robotpad) * num
	}
	println(sum)
}
