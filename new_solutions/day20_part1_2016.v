import os

struct Range {
	start int
	end   int
}

fn main() {
	mut ranges := []Range{}
	lines := os.read_lines('input.txt') or { return }

	for line in lines {
		parts := line.split('-')
		start := parts[0].int()
		end := parts[1].int()
		ranges << Range{start, end}
	}

	ranges.sort_with_compare(fn (a &Range, b &Range) int {
		if a.start < b.start || (a.start == b.start && a.end < b.end) {
			return -1
		} else if a.start == b.start && a.end == b.end {
			return 0
		} else {
			return 1
		}
	})

	mut lowest_unblocked := 0
	for r in ranges {
		if lowest_unblocked < r.start {
			break
		}
		if lowest_unblocked <= r.end {
			lowest_unblocked = r.end + 1
		}
	}

	println(lowest_unblocked)
}