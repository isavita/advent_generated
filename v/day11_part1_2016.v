
import os

const max_floors = 4
const max_materials = 7
const max_items = max_materials * 2

struct Item {
    material_id int
    is_chip     bool
}

struct State {
    floors       [max_floors][max_items]Item
    floor_counts [max_floors]int
    elevator     int
    steps        int
    materials    int
}

fn main() {
    mut state := State{}
    mut mat_map := map[string]int{}
    mut mat_buf := ''
    mut floor := 0

    txt := os.read_file('input.txt') or { panic(err) }
    for line in txt.split_into_lines() {
        if floor == max_floors { break }
        for tok in line.split(' \t\n.,;-') {
            match tok {
                'generator' {
                    id := mat_map[mat_buf] or {
                        mat_map[mat_buf] = state.materials
                        state.materials++
                        state.materials - 1
                    }
                    state.floors[floor][state.floor_counts[floor]] = Item{id, false}
                    state.floor_counts[floor]++
                }
                'microchip' {
                    id := mat_map[mat_buf] or {
                        mat_map[mat_buf] = state.materials
                        state.materials++
                        state.materials - 1
                    }
                    state.floors[floor][state.floor_counts[floor]] = Item{id, true}
                    state.floor_counts[floor]++
                }
                'relevant', 'a', 'and', 'floor', 'contains', 'The',
                'nothing', 'first', 'second', 'third', 'fourth', 'compatible' {}
                else {
                    mat_buf = tok.replace('-compatible', '')
                }
            }
        }
        floor++
    }

    mut queue := [state]
    mut seen := map[u64]bool{}

    for queue.len > 0 {
        cur := queue.pop()
        if cur.floors[0].len == 0 && cur.floors[1].len == 0 && cur.floors[2].len == 0 {
            println(cur.steps)
            return
        }

        mut gen := [max_materials]int{init: -1}
        mut chip := [max_materials]int{init: -1}
        for f in 0 .. max_floors {
            for i in 0 .. cur.floor_counts[f] {
                it := cur.floors[f][i]
                if it.is_chip {
                    chip[it.material_id] = f
                } else {
                    gen[it.material_id] = f
                }
            }
        }

        mut pairs := []Pair{cap: state.materials}
        for m in 0 .. state.materials {
            pairs << Pair{gen[m], chip[m]}
        }
        pairs.sort()
        mut h := u64(cur.elevator)
        for p in pairs {
            h = h * 31 + u64(p.gen)
            h = h * 31 + u64(p.chip)
        }
        if seen.exists(h) { continue }
        seen[h] = true

        for d in [-1, 1] {
            nxt := cur.elevator + d
            if nxt < 0 || nxt >= max_floors { continue }
            for i in 0 .. cur.floor_counts[cur.elevator] {
                for j in i .. cur.floor_counts[cur.elevator] {
                    mut s := cur
                    s.elevator = nxt
                    s.steps++
                    s.floor_counts[cur.elevator] -= 1 + (i != j)
                    s.floor_counts[nxt] += 1 + (i != j)
                    a := cur.floors[cur.elevator][i]
                    b := cur.floors[cur.elevator][j]
                    s.floors[nxt][s.floor_counts[nxt] - 1 - (i != j)] = a
                    if i != j {
                        s.floors[nxt][s.floor_counts[nxt] - 1] = b
                    }
                    if valid(s) { queue << s }
                }
            }
        }
    }
    println('No solution')
}

struct Pair {
    gen int
    chip int
}

fn valid(s State) bool {
    mut gen := [max_materials]bool{}
    for f in 0 .. max_floors {
        mut has_gen := false
        for i in 0 .. s.floor_counts[f] {
            if !s.floors[f][i].is_chip {
                gen[s.floors[f][i].material_id] = true
                has_gen = true
            }
        }
        if !has_gen { continue }
        for i in 0 .. s.floor_counts[f] {
            it := s.floors[f][i]
            if it.is_chip && !gen[it.material_id] { return false }
        }
    }
    return true
}
