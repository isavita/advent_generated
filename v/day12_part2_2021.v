
import os

const max_nodes = 20
const max_name_len = 10

struct Graph {
mut:
	nodes      []string
	adj_matrix [][]int
	node_count int
}

fn find_or_add_node(mut g Graph, name string) int {
	for i in 0 .. g.node_count {
		if g.nodes[i] == name {
			return i
		}
	}
	g.nodes << name
	g.node_count++
	return g.node_count - 1
}

fn add_edge(mut g Graph, from string, to string) {
	from_idx := find_or_add_node(mut g, from)
	to_idx := find_or_add_node(mut g, to)
	g.adj_matrix[from_idx][to_idx] = 1
	g.adj_matrix[to_idx][from_idx] = 1
}

fn is_lower(name string) bool {
	for c in name {
		if c < `a` || c > `z` {
			return false
		}
	}
	return true
}

fn walk(mut g Graph, curr int, mut visited []int, double_used bool) int {
	if g.nodes[curr] == 'end' {
		return 1
	}
	visited[curr]++
	mut paths := 0
	for next in 0 .. g.node_count {
		if g.adj_matrix[curr][next] == 0 {
			continue
		}
		if g.nodes[next] == 'start' {
			continue
		}
		mut du := double_used
		if is_lower(g.nodes[next]) && visited[next] > 0 {
			if du {
				continue
			} else {
				du = true
			}
		}
		paths += walk(mut g, next, mut visited, du)
		visited[next]--
		if is_lower(g.nodes[next]) && visited[next] == 1 {
			du = false
		}
	}
	return paths
}

fn main() {
	content := os.read_file('input.txt') or { panic(err) }
	mut g := Graph{
		adj_matrix: [][]int{len: max_nodes, init: []int{len: max_nodes}}
	}
	for line in content.split_into_lines() {
		parts := line.split('-')
		add_edge(mut g, parts[0], parts[1])
	}
	mut visited := []int{len: max_nodes}
	start := find_or_add_node(mut g, 'start')
	println(walk(mut g, start, mut visited, false))
}
