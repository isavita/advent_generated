import os

fn main() {
	mut east := 0
	mut north := 0
	mut direction := 0 // 0 = east, 90 = south, 180 = west, 270 = north

	lines := os.read_lines('input.txt') or { return }
	for line in lines {
		action := line[0]
		value := line[1..].int()

		match action {
			`N` { north += value }
			`S` { north -= value }
			`E` { east += value }
			`W` { east -= value }
			`L` { direction = (direction - value + 360) % 360 }
			`R` { direction = (direction + value) % 360 }
			`F` {
				match direction {
					0 { east += value }
					90 { north -= value }
					180 { east -= value }
					270 { north += value }
					else {}
				}
			}
			else {}
		}
	}

	println(abs(east) + abs(north))
}

fn abs(x int) int {
	return if x < 0 { -x } else { x }
}