
import os

struct Range {
	dest_start i64
	src_start  i64
	length     i64
}

fn reverse_convert(n i64, ranges []Range) i64 {
	for r in ranges {
		if r.dest_start <= n && n < r.dest_start + r.length {
			return r.src_start + (n - r.dest_start)
		}
	}
	return n
}

fn in_seed_ranges(n i64, seed_ranges []i64) bool {
	for i := 0; i < seed_ranges.len; i += 2 {
		start, len := seed_ranges[i], seed_ranges[i+1]
		if start <= n && n < start + len {
			return true
		}
	}
	return false
}

fn main() {
	raw := os.read_file('input.txt') or { panic('missing input.txt') }
	lines := raw.split_into_lines()

	mut seed_ranges := []i64{}
	mut maps := [][]Range{}

	for line in lines {
		if line.len == 0 { continue }
		if line.starts_with('seeds:') {
			mut nums := line[7..].split(' ')
			for n in nums {
				seed_ranges << n.i64()
			}
			continue
		}
		if line.ends_with('map:') {
			maps << []Range{}
			continue
		}
		parts := line.split(' ')
		if parts.len == 3 {
			maps[maps.len-1] << Range{
				dest_start: parts[0].i64()
				src_start:  parts[1].i64()
				length:     parts[2].i64()
			}
		}
	}

	for loc := i64(0); ; loc++ {
		mut seed := loc
		for i := maps.len - 1; i >= 0; i-- {
			seed = reverse_convert(seed, maps[i])
		}
		if in_seed_ranges(seed, seed_ranges) {
			println(loc)
			break
		}
	}
}
