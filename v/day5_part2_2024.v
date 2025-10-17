
import os

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut rules := [][]int{len: 0, cap: 128}
    mut updates := [][]int{len: 0, cap: 128}

    mut rule_section := true
    for line in txt.split_into_lines() {
        if line == '' { continue }
        if line.contains('|') {
            if !rule_section { continue }
            parts := line.split('|')
            rules << [parts[0].int(), parts[1].int()]
        } else {
            rule_section = false
            updates << line.split(',').map(it.int())
        }
    }

    mut sum := 0
    for pages in updates {
        mut idx := map[int]int{}
        for i, p in pages { idx[p] = i }

        mut ok := true
        for r in rules {
            x, y := r[0], r[1]
            if x in idx && y in idx && idx[x] > idx[y] {
                ok = false
                break
            }
        }

        if !ok {
            mut deg := map[int]int{}
            mut adj := map[int][]int{}
            for p in pages {
                deg[p] = 0
                adj[p] = []int{}
            }
            for r in rules {
                x, y := r[0], r[1]
                if x in deg && y in deg {
                    adj[x] << y
                    deg[y]++
                }
            }

            mut q := []int{}
            for p in pages {
                if deg[p] == 0 { q << p }
            }
            mut sorted := []int{cap: pages.len}
            for q.len > 0 {
                v := q.pop()
                sorted << v
                for u in adj[v] {
                    deg[u]--
                    if deg[u] == 0 { q << u }
                }
            }
            sum += sorted[sorted.len / 2]
        }
    }
    println(sum)
}
