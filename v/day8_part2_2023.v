
import os

const max_nodes = 1000
const max_instr_len = 1000
const node_name_len = 3
const hash_size = 26 * 26 * 26

struct Node {
	name string
	left string
	right string
}

struct Context {
mut:
	nodes []Node
	node_map []int
	instructions string
}

fn name_to_int(name string) int {
	if name.len != node_name_len { return -1 }
	return int(name[0] - `A`) * 26 * 26 + int(name[1] - `A`) * 26 + int(name[2] - `A`)
}

fn gcd(a i64, b i64) i64 {
	mut x, mut y := a, b
	for y != 0 {
		x, y = y, x % y
	}
	return x
}

fn lcm(a i64, b i64) i64 {
	if a == 0 || b == 0 { return 0 }
	return (a / gcd(a, b)) * b
}

fn lcm_array(arr []i64) i64 {
	if arr.len == 0 { return 0 }
	mut res := arr[0]
	for i := 1; i < arr.len; i++ {
		res = lcm(res, arr[i])
	}
	return res
}

fn parse_input(filename string) Context {
	content := os.read_file(filename) or { panic(err) }
	lines := content.split_into_lines()
	mut ctx := Context{
		nodes: []Node{cap: max_nodes}
		node_map: []int{len: hash_size, init: -1}
	}
	ctx.instructions = lines[0]
	for line in lines[2..] {
		if line.len < 10 { continue }
		parts := line.split(' = ')
		if parts.len != 2 { continue }
		name := parts[0]
		right := parts[1].trim('()')
		lr := right.split(', ')
		if lr.len != 2 { continue }
		ctx.nodes << Node{name, lr[0], lr[1]}
	}
	for i, node in ctx.nodes {
		hash := name_to_int(node.name)
		if hash >= 0 && hash < hash_size {
			ctx.node_map[hash] = i
		}
	}
	return ctx
}

fn solve(ctx Context) i64 {
	mut starts := []int{}
	for i, node in ctx.nodes {
		if node.name.ends_with('A') {
			starts << i
		}
	}
	if starts.len == 0 { return 0 }
	mut steps := []i64{len: starts.len}
	for i, start in starts {
		mut idx := start
		for !ctx.nodes[idx].name.ends_with('Z') {
			ins := ctx.instructions[int(steps[i]) % ctx.instructions.len]
			next := if ins == `L` { ctx.nodes[idx].left } else { ctx.nodes[idx].right }
			hash := name_to_int(next)
			if hash < 0 || hash >= hash_size || ctx.node_map[hash] == -1 {
				panic("Node not found: $next")
			}
			idx = ctx.node_map[hash]
			steps[i]++
		}
	}
	return lcm_array(steps)
}

fn main() {
	ctx := parse_input("input.txt")
	println(solve(ctx))
}
