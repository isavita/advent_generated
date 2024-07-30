import os

fn count_orbits(orbit_map map[string][]string, start string, depth int) int {
	orbits := orbit_map[start] or { return depth }
	mut count := depth
	for orbit in orbits {
		count += count_orbits(orbit_map, orbit, depth + 1)
	}
	return count
}

fn main() {
	data := os.read_file('input.txt') or { panic(err) }
	lines := data.split('\n').filter(it.len > 0)
	mut orbit_map := map[string][]string{}
	for line in lines {
		parts := line.split(')')
		center := parts[0]
		orbiter := parts[1]
		orbit_map[center] << orbiter
	}
	total_orbits := count_orbits(orbit_map, 'COM', 0)
	println(total_orbits)
}