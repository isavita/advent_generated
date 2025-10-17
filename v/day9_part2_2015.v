
import os

const max_locations = 100
const max_name_len = 20

struct Location {
mut:
	name      string
	distances []int
}

fn main() {
	mut locations := []Location{cap: max_locations}
	mut index := map[string]int{}

	mut data := os.read_file('input.txt') or { panic('missing input.txt') }
	for line in data.split_into_lines() {
		parts := line.split(' ')
		from := parts[0]
		to := parts[2]
		distance := parts[4].int()

		mut fi := index[from] or {
			index[from] = locations.len
			locations << Location{name: from, distances: []int{len: max_locations}}
			locations.len - 1
		}
		mut ti := index[to] or {
			index[to] = locations.len
			locations << Location{name: to, distances: []int{len: max_locations}}
			locations.len - 1
		}
		locations[fi].distances[ti] = distance
		locations[ti].distances[fi] = distance
	}

	mut route := []int{len: locations.len}
	for i in 0 .. locations.len {
		route[i] = i
	}

	mut best := 0
	for {
		mut sum := 0
		for i in 0 .. locations.len - 1 {
			sum += locations[route[i]].distances[route[i + 1]]
		}
		if sum > best {
			best = sum
		}
		if !next_permutation(mut route) {
			break
		}
	}
	println(best)
}

fn next_permutation(mut a []int) bool {
	mut i := a.len - 1
	for i > 0 && a[i - 1] >= a[i] {
		i--
	}
	if i == 0 {
		return false
	}
	mut j := a.len - 1
	for a[j] <= a[i - 1] {
		j--
	}
	a[i - 1], a[j] = a[j], a[i - 1]
	mut l := i
	mut r := a.len - 1
	for l < r {
		a[l], a[r] = a[r], a[l]
		l++
		r--
	}
	return true
}
