import os

fn main() {
	mut bag_map := map[string][]string{}
	input := os.read_file('input.txt') or { return }

	for line in input.split('\n') {
		if line.trim_space() == '' { continue }
		parts := line.split(' bags contain ')
		outer_bag := parts[0]
		inner_bags := parts[1].split(', ')
		for inner in inner_bags {
			if inner.contains('no other bags') { continue }
			inner_bag := inner.split(' ')[1..3].join(' ').replace(' bags', '').replace(' bag', '')
			bag_map[inner_bag] << outer_bag
		}
	}

	mut visited := map[string]bool{}
	count := count_bags('shiny gold', mut bag_map, mut visited)
	println(count)
}

fn count_bags(target string, mut bag_map map[string][]string, mut visited map[string]bool) int {
	mut count := 0
	for outer in bag_map[target] {
		if !visited[outer] {
			visited[outer] = true
			count += 1 + count_bags(outer, mut bag_map, mut visited)
		}
	}
	return count
}