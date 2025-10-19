
import os

fn apply_mask(value u64, mask string) u64 {
    mut res := u64(0)
    for i := 0; i < 36; i++ {
        bit := u64(1) << (35 - i)
        match mask[i] {
            `1` { res |= bit }
            `X` { res |= value & bit }
            else {}
        }
    }
    return res
}

fn main() {
    txt := os.read_file('input.txt') or { panic(err) }
    mut mask := ''
    mut mem := map[u64]u64{}
    for line in txt.split_into_lines() {
        if line.starts_with('mask = ') {
            mask = line[7..]
        } else {
            mut addr := u64(0)
            mut val := u64(0)
            if line.len > 6 && line[0..4] == 'mem[' {
                addr = line[4..line.index(']') or { continue }].u64()
                val = line[line.index('=') or { continue } + 2..].u64()
                mem[addr] = apply_mask(val, mask)
            }
        }
    }
    mut sum := u64(0)
    for _, v in mem {
        sum += v
    }
    println(sum)
}
