
import os

struct Coord {
mut:
	x int
	y int
}

fn (a Coord) + (b Coord) Coord {
	return Coord{a.x + b.x, a.y + b.y}
}

fn (c Coord) rotate90() Coord {
	return Coord{c.y, -c.x}
}

fn (c Coord) rotate_neg90() Coord {
	return Coord{-c.y, c.x}
}

struct Beam {
mut:
	origin Coord
	dir   Coord
}

struct Grid {
mut:
	width  int
	height int
	data   []string
}

fn grid_from(lines []string) Grid {
	h := lines.len
	w := if h > 0 { lines[0].len } else { 0 }
	return Grid{w, h, lines.clone()}
}

fn (g Grid) get(c Coord) u8 {
	return g.data[c.y][c.x]
}

fn (g Grid) in_bounds(c Coord) bool {
	return c.x >= 0 && c.x < g.width && c.y >= 0 && c.y < g.height
}

const (
	north = Coord{0, -1}
	south = Coord{0, 1}
	east  = Coord{1, 0}
	west  = Coord{-1, 0}
)

fn is_horizontal(dir Coord) bool {
	return dir == east || dir == west
}

fn is_vertical(dir Coord) bool {
	return dir == north || dir == south
}

fn next_beam(g Grid, b Beam) []Beam {
	mut res := []Beam{}
	cell := g.get(b.origin)
	match cell {
		`.` { res << Beam{b.origin + b.dir, b.dir} }
		`/` {
			new_dir := if is_vertical(b.dir) { b.dir.rotate_neg90() } else { b.dir.rotate90() }
			res << Beam{b.origin + new_dir, new_dir}
		}
		`\\` {
			new_dir := if is_vertical(b.dir) { b.dir.rotate90() } else { b.dir.rotate_neg90() }
			res << Beam{b.origin + new_dir, new_dir}
		}
		`|` {
			if is_horizontal(b.dir) {
				d1 := b.dir.rotate90()
				d2 := b.dir.rotate_neg90()
				res << Beam{b.origin + d1, d1}
				res << Beam{b.origin + d2, d2}
			} else {
				res << Beam{b.origin + b.dir, b.dir}
			}
		}
		`-` {
			if is_vertical(b.dir) {
				d1 := b.dir.rotate90()
				d2 := b.dir.rotate_neg90()
				res << Beam{b.origin + d1, d1}
				res << Beam{b.origin + d2, d2}
			} else {
				res << Beam{b.origin + b.dir, b.dir}
			}
		}
		else {}
	}
	return res
}

fn calculate_propagation(g Grid, start Beam) map[u64]bool {
	mut seen := map[u64]bool{}
	mut queue := [start]
	for queue.len > 0 {
		b := queue[0]
		queue = queue[1..]
		if !g.in_bounds(b.origin) {
			continue
		}
		key := u64(b.origin.x) | (u64(b.origin.y) << 16) | (u64(b.dir.x) << 32) | (u64(b.dir.y) << 48)
		if seen[key] {
			continue
		}
		seen[key] = true
		for nb in next_beam(g, b) {
			queue << nb
		}
	}
	return seen
}

fn energization(seen map[u64]bool) int {
	mut coords := map[u64]bool{}
	for k, _ in seen {
		coords[u64(k & 0xffff) | (u64((k >> 16) & 0xffff) << 16)] = true
	}
	return coords.len
}

fn solve(lines []string) int {
	g := grid_from(lines)
	start := Beam{Coord{0, 0}, east}
	seen := calculate_propagation(g, start)
	return energization(seen)
}

fn main() {
	txt := os.read_file('input.txt') or { return }
	lines := txt.split_into_lines()
	println(solve(lines))
}
