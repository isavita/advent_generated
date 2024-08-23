import os

fn count_adjacent(area [][]u8, x int, y int) (int, int, int) {
	mut open := 0
	mut trees := 0
	mut lumberyards := 0
	for dy in [-1, 0, 1] {
		for dx in [-1, 0, 1] {
			if dx == 0 && dy == 0 {
				continue
			}
			nx := x + dx
			ny := y + dy
			if nx >= 0 && nx < area[0].len && ny >= 0 && ny < area.len {
				match area[ny][nx] {
					`.` { open++ }
					`|` { trees++ }
					`#` { lumberyards++ }
					else {}
				}
			}
		}
	}
	return open, trees, lumberyards
}

fn simulate(area [][]u8) [][]u8 {
	mut new_area := area.clone()
	for y in 0 .. area.len {
		for x in 0 .. area[0].len {
			open, trees, lumberyards := count_adjacent(area, x, y)
			match area[y][x] {
				`.` {
					if trees >= 3 {
						new_area[y][x] = `|`
					}
				}
				`|` {
					if lumberyards >= 3 {
						new_area[y][x] = `#`
					}
				}
				`#` {
					if lumberyards < 1 || trees < 1 {
						new_area[y][x] = `.`
					}
				}
				else {}
			}
		}
	}
	return new_area
}

fn main() {
	mut area := [][]u8{}
	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		area << line.bytes()
	}

	for _ in 0 .. 10 {
		area = simulate(area)
	}

	mut wooded := 0
	mut lumberyards := 0
	for row in area {
		for acre in row {
			match acre {
				`|` { wooded++ }
				`#` { lumberyards++ }
				else {}
			}
		}
	}

	println(wooded * lumberyards)
}