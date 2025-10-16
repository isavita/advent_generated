
import os

struct RangeMap {
	src_start  i64
	dest_start i64
	length     i64
}

fn convert_number(n i64, ranges []RangeMap) i64 {
	for r in ranges {
		if n >= r.src_start && n < r.src_start + r.length {
			return r.dest_start + (n - r.src_start)
		}
	}
	return n
}

fn main() {
	raw := os.read_file('input.txt') or { panic(err) }
	lines := raw.split_into_lines()

	mut seeds := []i64{}
	mut maps := [][]RangeMap{len: 10, init: []RangeMap{cap: 100}}
	mut map_idx := -1

	for line in lines {
		if line.contains('map:') {
			map_idx++
		} else if line.starts_with('seeds:') {
			parts := line[7..].split(' ')
			for p in parts {
				seeds << p.i64()
			}
		} else {
			fields := line.split(' ')
			if fields.len == 3 {
				maps[map_idx] << RangeMap{
					dest_start: fields[0].i64()
					src_start: fields[1].i64()
					length: fields[2].i64()
				}
			}
		}
	}

	mut min_loc := i64(9223372036854775807)
	for s in seeds {
		mut loc := s
		for j in 0 .. map_idx + 1 {
			loc = convert_number(loc, maps[j])
		}
		if loc < min_loc {
			min_loc = loc
		}
	}
	println(min_loc)
}
