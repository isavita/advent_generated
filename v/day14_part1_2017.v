
import os

fn reverse_section(mut arr []int, start int, length int) {
    n := 256
    for i, j := start, start + length - 1; i < j; i, j = i + 1, j - 1 {
        arr[i % n], arr[j % n] = arr[j % n], arr[i % n]
    }
}

fn knot_hash(input string) string {
    mut lengths := input.bytes()
    lengths << [u8(17), 31, 73, 47, 23]
    len := lengths.len

    mut list := []int{len: 256}
    for i in 0 .. 256 {
        list[i] = i
    }

    mut pos := 0
    mut skip := 0
    for _ in 0 .. 64 {
        for i in 0 .. len {
            reverse_section(mut list, pos, int(lengths[i]))
            pos += int(lengths[i]) + skip
            skip++
        }
    }

    mut dense := []int{len: 16}
    for i in 0 .. 16 {
        mut xor := 0
        for j in 0 .. 16 {
            xor ^= list[i * 16 + j]
        }
        dense[i] = xor
    }

    mut result := []u8{cap: 32}
    for b in dense {
        result << hex(b >> 4)
        result << hex(b & 0xF)
    }
    return result.bytestr()
}

fn hex(n int) u8 {
    return if n < 10 { u8(n + 48) } else { u8(n + 87) }
}

fn main() {
    key := os.read_file('input.txt') or { panic('file') }.trim_space()
    mut total := 0
    for i in 0 .. 128 {
        hash := knot_hash('${key}-${i}')
        for c in hash {
            mut v := match c {
                `0`...`9` { int(c - 48) }
                else { int(c - 87) }
            }
            for v > 0 {
                total += v & 1
                v >>= 1
            }
        }
    }
    println(total)
}
