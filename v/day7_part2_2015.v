
import os

struct WireRule {
mut:
	wire string
	rule string
}

fn find_wire_index(wires []WireRule, wire string) int {
	for i, w in wires {
		if w.wire == wire {
			return i
		}
	}
	return -1
}

fn memo_dfs(wires []WireRule, entry string, mut memo map[string]u16) u16 {
	if entry[0].is_digit() {
		return entry.u16()
	}
	if entry in memo {
		return memo[entry]
	}
	idx := find_wire_index(wires, entry)
	if idx == -1 {
		return 0
	}
	parts := wires[idx].rule.split(' ')
	mut res := u16(0)
	match parts.len {
		1 {
			res = memo_dfs(wires, parts[0], mut memo)
		}
		2 {
			res = ~memo_dfs(wires, parts[1], mut memo)
		}
		3 {
			l := memo_dfs(wires, parts[0], mut memo)
			r := memo_dfs(wires, parts[2], mut memo)
			res = match parts[1] {
				'AND' { l & r }
				'OR'  { l | r }
				'LSHIFT' { l << r }
				'RSHIFT' { l >> r }
				else { 0 }
			}
		}
		else {}
	}
	memo[entry] = res
	return res
}

fn main() {
	text := os.read_file('input.txt') or { panic(err) }
	lines := text.trim_space().split_into_lines()
	mut wires := []WireRule{}
	for line in lines {
		parts := line.split(' -> ')
		wires << WireRule{parts[1], parts[0]}
	}
	mut memo := map[string]u16{}
	a := memo_dfs(wires, 'a', mut memo)
	b_idx := find_wire_index(wires, 'b')
	if b_idx != -1 {
		wires[b_idx].rule = a.str()
	}
	memo.clear()
	println(memo_dfs(wires, 'a', mut memo))
}
