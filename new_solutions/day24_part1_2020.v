import os

struct Point {
	mut:
		x int
		y int
}

fn main() {
	mut black_tiles := map[string]int{}
	input := os.read_file('input.txt') or { return }

	for line in input.split('\n') {
		if line.len == 0 {
			continue
		}
		mut pos := Point{0, 0}
		mut i := 0
		for i < line.len {
			match line[i] {
				`e` { pos.x++ }
				`w` { pos.x-- }
				`n` {
					i++
					match line[i] {
						`e` { pos.y--; pos.x++ }
						`w` { pos.y-- }
						else {}
					}
				}
				`s` {
					i++
					match line[i] {
						`e` { pos.y++ }
						`w` { pos.y++; pos.x-- }
						else {}
					}
				}
				else {}
			}
			i++
		}
		key := '${pos.x},${pos.y}'
		black_tiles[key] = if black_tiles[key] == 1 { 0 } else { 1 }
	}

	mut count := 0
	for _, v in black_tiles {
		if v == 1 {
			count++
		}
	}
	println(count)
}