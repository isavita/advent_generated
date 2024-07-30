import os

fn find_combinations(containers []int, target int, index int, count int, min_count int, ways int) (int, int) {
	if target == 0 {
		if min_count == 0 || count < min_count {
			return count, 1
		} else if count == min_count {
			return min_count, ways + 1
		}
		return min_count, ways
	}
	if target < 0 || index >= containers.len {
		return min_count, ways
	}
	mut new_min_count, mut new_ways := min_count, ways
	new_min_count, new_ways = find_combinations(containers, target - containers[index], index + 1, count + 1, new_min_count, new_ways)
	new_min_count, new_ways = find_combinations(containers, target, index + 1, count, new_min_count, new_ways)
	return new_min_count, new_ways
}

fn main() {
	file := os.read_file('input.txt') or { return }
	containers := file.split('\n').map(it.int()).filter(it != 0)

	mut min_count := 0
	mut ways := 0
	min_count, ways = find_combinations(containers, 150, 0, 0, min_count, ways)
	println(ways)
}