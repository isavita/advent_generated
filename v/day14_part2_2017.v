
import os

fn reverse_section(mut arr []int, start int, length int, n int) {
    mut i := start
    mut j := start + length - 1
    for i < j {
        arr[i % n], arr[j % n] = arr[j % n], arr[i % n]
        i++
        j--
    }
}

fn knot_hash(input string) string {
    mut lengths := []int{cap: 256 + 5}
    for b in input.bytes() {
        lengths << int(b)
    }
    lengths << 17
    lengths << 31
    lengths << 73
    lengths << 47
    lengths << 23

    mut list := []int{cap: 256}
    for i in 0 .. 256 {
        list << i
    }

    mut pos := 0
    mut skip := 0
    for _ in 0 .. 64 {
        for l in lengths {
            reverse_section(mut list, pos, l, 256)
            pos = (pos + l + skip) % 256
            skip++
        }
    }

    mut dense := []int{cap: 16}
    for i in 0 .. 16 {
        mut xor := 0
        for j in 0 .. 16 {
            xor ^= list[i * 16 + j]
        }
        dense << xor
    }

    mut res := []u8{cap: 32}
    for d in dense {
        res << u8((d >> 4) & 0xf) + (if (d >> 4) & 0xf < 10 { u8(`0`) } else { u8(`a`) - 10 })
        res << u8(d & 0xf) + (if d & 0xf < 10 { u8(`0`) } else { u8(`a`) - 10 })
    }
    return res.bytestr()
}

fn dfs(x int, y int, mut grid [][]int) {
    if x < 0 || x >= 128 || y < 0 || y >= 128 || grid[x][y] != 1 {
        return
    }
    grid[x][y] = 0
    dfs(x - 1, y, mut grid)
    dfs(x + 1, y, mut grid)
    dfs(x, y - 1, mut grid)
    dfs(x, y + 1, mut grid)
}

fn main() {
    key := os.read_file('input.txt') or { panic('file') }.trim_space()
    mut grid := [][]int{len: 128, init: []int{len: 128}}

    for i in 0 .. 128 {
        row_key := '${i}'
        hash := knot_hash(key + '-' + row_key)
        for j in 0 .. 128 {
            hex := hash[j / 4]
            bit := if hex >= `0` && hex <= `9` { hex - `0` } else { hex - `a` + 10 }
            grid[i][j] = if (bit >> (3 - j % 4)) & 1 == 1 { 1 } else { 0 }
        }
    }

    mut regions := 0
    for i in 0 .. 128 {
        for j in 0 .. 128 {
            if grid[i][j] == 1 {
                regions++
                dfs(i, j, mut grid)
            }
        }
    }
    println(regions)
}
