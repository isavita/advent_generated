import os

fn read_input(filename string) map[int][]int {
	mut connections := map[int][]int{}
	lines := os.read_lines(filename) or { return connections } // Handle potential errors
	for line in lines {
		parts := line.split(' <-> ')
		prog_id := parts[0].int()
		connected := parts[1].split(', ').map(it.int())
		connections[prog_id] = connected
	}
	return connections
}

fn dfs(prog_id int, connections map[int][]int, mut visited map[int]bool) {
	visited[prog_id] = true
	for neighbor in connections[prog_id] {
		if !visited[neighbor] {
			dfs(neighbor, connections, mut visited)
		}
	}
}

fn count_group_size(prog_id int, connections map[int][]int) int {
	mut visited := map[int]bool{}
	dfs(prog_id, connections, mut visited)
	return visited.len
}

fn count_total_groups(connections map[int][]int) int {
	mut visited := map[int]bool{}
	mut group_count := 0
	for prog_id in connections.keys() {
		if !visited[prog_id] {
			dfs(prog_id, connections, mut visited)
			group_count++
		}
	}
	return group_count
}

fn main() {
	connections := read_input('input.txt')
	group_size := count_group_size(0, connections)
	total_groups := count_total_groups(connections)
	println('Size of group containing program ID 0: $group_size')
	println('Total number of groups: $total_groups')
}