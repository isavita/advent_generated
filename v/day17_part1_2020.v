
import os

const max_size = 50
const cycles = 6

struct Coord {
	x int
	y int
	z int
}

struct ActiveCubes {
mut:
	coords []Coord
}

fn (mut a ActiveCubes) add(c Coord) {
	a.coords << c
}

fn (a ActiveCubes) active(c Coord) bool {
	for t in a.coords {
		if t.x == c.x && t.y == c.y && t.z == c.z {
			return true
		}
	}
	return false
}

fn simulate(mut old ActiveCubes) ActiveCubes {
	mut cnt := [][][]int{len: max_size, init: [][]int{len: max_size, init: []int{len: max_size}}}
	mut minx, mut maxx := max_size, -max_size
	mut miny, mut maxy := max_size, -max_size
	mut minz, mut maxz := max_size, -max_size
	for c in old.coords {
		if c.x < minx { minx = c.x }
		if c.x > maxx { maxx = c.x }
		if c.y < miny { miny = c.y }
		if c.y > maxy { maxy = c.y }
		if c.z < minz { minz = c.z }
		if c.z > maxz { maxz = c.z }
		for dz in -1 .. 2 {
			for dy in -1 .. 2 {
				for dx in -1 .. 2 {
					if dx == 0 && dy == 0 && dz == 0 { continue }
					cnt[c.x + dx + max_size / 2][c.y + dy + max_size / 2][c.z + dz + max_size / 2]++
				}
			}
		}
	}
	mut n := ActiveCubes{}
	for z in minz - 1 .. maxz + 2 {
		for y in miny - 1 .. maxy + 2 {
			for x in minx - 1 .. maxx + 2 {
				c := Coord{ x, y, z }
				nx := x + max_size / 2
				ny := y + max_size / 2
				nz := z + max_size / 2
				if cnt[nx][ny][nz] == 3 || (cnt[nx][ny][nz] == 2 && old.active(c)) {
					n.add(c)
				}
			}
		}
	}
	return n
}

fn main() {
	buf := os.read_file('input.txt') or { panic(err) }
	mut active := ActiveCubes{}
	mut y := 0
	for line in buf.split_into_lines() {
		for x, ch in line {
			if ch == `#` {
				active.add(Coord{ x, y, 0 })
			}
		}
		y++
	}
	for _ in 0 .. cycles {
		active = simulate(mut active)
	}
	println(active.coords.len)
}
