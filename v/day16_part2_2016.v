
import os

const disk_length = 35651584

fn main() {
    initial := os.read_file('input.txt') or { panic(err) }.trim_space()
    mut data := []u8{cap: disk_length}
    data << initial.bytes()
    for data.len < disk_length {
        data << u8(`0`)
        for i := data.len - 2; i >= 0 && data.len < disk_length; i-- {
            data << if data[i] == `0` { `1` } else { `0` }
        }
    }
    data = data[..disk_length]
    mut checksum := data.clone()
    for checksum.len % 2 == 0 {
        mut j := 0
        for i := 0; i < checksum.len; i += 2 {
            checksum[j] = if checksum[i] == checksum[i + 1] { `1` } else { `0` }
            j++
        }
        checksum = checksum[..j]
    }
    println(checksum.bytestr())
}
