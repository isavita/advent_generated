
import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    for line in data.split_into_lines() {
        mut parts := line.split('[')
        if parts.len != 2 { continue }
        mut body := parts[0].split('-')
        checksum := parts[1][..5]
        if body.len < 2 { continue }
        sector := body.last().int()
        body.pop()
        mut freq := [26]int{}
        for name in body {
            for c in name {
                freq[c - `a`]++
            }
        }
        mut ok := true
        for i in 0 .. 5 {
            mut best := -1
            mut max := -1
            for j, v in freq {
                if v > max || (v == max && j < best) {
                    max = v
                    best = j
                }
            }
            if best < 0 || best + `a` != checksum[i] {
                ok = false
                break
            }
            freq[best] = 0
        }
        if !ok { continue }
        mut out := []u8{}
        for name in body {
            for c in name {
                out << u8(`a` + (c - `a` + sector) % 26)
            }
            out << ` `
        }
        if out.bytestr().contains('northpole object') {
            println(sector)
            break
        }
    }
}
