
import os
import strconv

// ---------- bit helpers ----------
fn popcnt(x u32) int {
    mut c := 0
    mut v := x
    for v != 0 {
        c += int(v & 1)
        v >>= 1
    }
    return c
}

// ---------- state representation ----------
// floors 0..3 (ground floor is 0 in code, 1 in description)
// each floor holds:
//   gens: bitset of generators
//   chips: bitset of micro-chips
// elevator position: 0..3
// total bits: 4*2*10 + 2 = 82, fits in u128
struct State {
mut:
    elev u8
    gens [4]u16
    chips [4]u16
}

fn (s State) hash() u128 {
    mut h := u128(s.elev)
    for f in 0..4 {
        h = (h << 20) | u128(s.gens[f])<<10 | u128(s.chips[f])
    }
    return h
}

// canonicalise: sort floors by (gens,chips) pair so that isomorphic
// states (swapping element names) collapse to the same hash
fn (s State) canonical() State {
    mut pairs := []u16{len: 10}
    for f in 0..4 {
        g := s.gens[f]
        c := s.chips[f]
        for t in 0..10 {
            if (g & (u16(1)<<t)) != 0 && (c & (u16(1)<<t)) != 0 {
                pairs[t] = 1
            }
        }
    }
    // build renumbering map: first encountered pair gets idx 0, next 1, …
    mut map := [10]u8{init: 10} // 10 means “unused”
    mut nxt := u8(0)
    for t in 0..10 {
        if pairs[t] == 1 {
            map[t] = nxt
            nxt++
        }
    }
    // apply mapping
    mut st := State{elev: s.elev}
    for f in 0..4 {
        mut ng := u16(0)
        mut nc := u16(0)
        for t in 0..10 {
            bit := u16(1) << t
            if (s.gens[f] & bit) != 0 {
                ng |= u16(1) << map[t]
            }
            if (s.chips[f] & bit) != 0 {
                nc |= u16(1) << map[t]
            }
        }
        st.gens[f] = ng
        st.chips[f] = nc
    }
    return st
}

// is floor f safe?
fn (s State) safe_floor(f int) bool {
    g := s.gens[f]
    c := s.chips[f]
    // if no generators, always safe
    if g == 0 { return true }
    // every chip on this floor must have its generator here
    return (c & ~g) == 0
}

fn (s State) valid() bool {
    return s.safe_floor(0) && s.safe_floor(1) && s.safe_floor(2) && s.safe_floor(3)
}

// ---------- parsing ----------
fn parse_line(line string) (u16, u16) {
    mut gens := u16(0)
    mut chips := u16(0)
    words := line.split(' ')
    for w in words {
        if w.ends_with('-compatible') {
            // microchip
            elt := w[..w.len-11]
            idx := match elt {
                'hydrogen' { 0 }
                'lithium'  { 1 }
                'promethium'{2}
                'cobalt'   { 3 }
                'polonium' { 4 }
                'thulium'  { 5 }
                'elerium'  { 6 }
                'dilithium'{ 7 }
                else { continue }
            }
            chips |= u16(1) << idx
        } else if w.ends_with('generator') {
            // generator
            elt := w[..w.len-9]
            idx := match elt {
                'hydrogen' { 0 }
                'lithium'  { 1 }
                'promethium'{2}
                'cobalt'   { 3 }
                'polonium' { 4 }
                'thulium'  { 5 }
                'elerium'  { 6 }
                'dilithium'{ 7 }
                else { continue }
            }
            gens |= u16(1) << idx
        }
    }
    return gens, chips
}

// ---------- BFS ----------
struct QueueItem {
    state State
    dist int
}

fn solve(initial State) int {
    mut q := []QueueItem{}
    mut seen := map[u128]int{}
    q << QueueItem{initial, 0}
    seen[initial.canonical().hash()] = 0
    target := u16(0x3ff) // all 10 bits set
    for q.len > 0 {
        cur := q[0]
        q = q[1..]
        // check if all items are on floor 3
        if cur.state.gens[3] == target && cur.state.chips[3] == target {
            return cur.dist
        }
        // generate moves
        f := cur.state.elev
        // collect items on current floor
        mut items := []u8{}
        g := cur.state.gens[f]
        c := cur.state.chips[f]
        for t in 0..10 {
            bit := u16(1) << t
            if (g & bit) != 0 { items << u8(t<<1) }
            if (c & bit) != 0 { items << u8(t<<1|1) }
        }
        // try moving 1 or 2 items up or down
        for dir in [-1, +1] {
            nf := f + dir
            if nf < 0 || nf > 3 { continue }
            // single item
            for i in 0..items.len {
                mut st := cur.state
                // remove from f
                t := items[i] >> 1
                is_chip := (items[i] & 1) != 0
                if is_chip {
                    st.chips[f] &= ~u16(1) << t
                    st.chips[nf] |= u16(1) << t
                } else {
                    st.gens[f] &= ~u16(1) << t
                    st.gens[nf] |= u16(1) << t
                }
                st.elev = u8(nf)
                if !st.valid() { continue }
                h := st.canonical().hash()
                if h in seen && seen[h] <= cur.dist + 1 { continue }
                seen[h] = cur.dist + 1
                q << QueueItem{st, cur.dist + 1}
            }
            // pair of items
            for i in 0..items.len {
                for j in i+1..items.len {
                    mut st := cur.state
                    for _, x in [items[i], items[j]] {
                        t := x >> 1
                        is_chip := (x & 1) != 0
                        if is_chip {
                            st.chips[f] &= ~u16(1) << t
                            st.chips[nf] |= u16(1) << t
                        } else {
                            st.gens[f] &= ~u16(1) << t
                            st.gens[nf] |= u16(1) << t
                        }
                    }
                    st.elev = u8(nf)
                    if !st.valid() { continue }
                    h := st.canonical().hash()
                    if h in seen && seen[h] <= cur.dist + 1 { continue }
                    seen[h] = cur.dist + 1
                    q << QueueItem{st, cur.dist + 1}
                }
            }
        }
    }
    return -1
}

// ---------- main ----------
fn main() {
    txt := os.read_file('input.txt') or { panic('no input.txt') }
    lines := txt.split_into_lines()
    mut st := State{elev: 0}
    for f in 0..4 {
        if f >= lines.len { break }
        g, c := parse_line(lines[f])
        st.gens[f] = g
        st.chips[f] = c
    }
    p1 := solve(st)
    println('Part 1: $p1')
    // add elerium & dilithium on floor 0
    st.gens[0] |= u16(0x3) << 8 // bits 8,9
    st.chips[0] |= u16(0x3) << 8
    p2 := solve(st)
    println('Part 2: $p2')
}
