
import os

fn dragon_curve(mut a []u8) {
    mut b := a.clone()
    for i := 0; i < b.len; i++ {
        b[b.len - 1 - i] = if a[i] == `0` { `1` } else { `0` }
    }
    a << `0`
    a << b
}

fn checksum(mut data []u8) string {
    mut buf := []u8{cap: data.len / 2 + 1}
    for buf.len % 2 == 0 {
        buf.clear()
        for i := 0; i < data.len; i += 2 {
            buf << if data[i] == data[i + 1] { `1` } else { `0` }
        }
        data = buf.clone()
    }
    return buf.bytestr()
}

fn main() {
    initial := os.read_file('input.txt') or { return }
    mut state := initial.trim_space().bytes()
    for state.len < 272 {
        dragon_curve(mut state)
    }
    state = state[..272]
    println(checksum(mut state))
}
