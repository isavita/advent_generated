
import os

const list_size = 256

fn main() {
    input := os.read_file('input.txt') or {
        eprintln('File reading error')
        exit(1)
    }

    mut lengths := []int{}
    for c in input {
        lengths << int(c)
    }
    lengths << [17, 31, 73, 47, 23]

    mut list := []int{len: list_size, init: index}

    mut pos := 0
    mut skip := 0

    for _ in 0 .. 64 {
        for l in lengths {
            for j in 0 .. l / 2 {
                a := (pos + j) % list_size
                b := (pos + l - 1 - j) % list_size
                list[a], list[b] = list[b], list[a]
            }
            pos = (pos + l + skip) % list_size
            skip++
        }
    }

    mut dense := []u8{len: 16}
    for i in 0 .. 16 {
        mut xor := u8(0)
        for j in 0 .. 16 {
            xor ^= u8(list[i * 16 + j])
        }
        dense[i] = xor
    }

    println(dense.hex())
}
