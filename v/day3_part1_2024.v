
import os

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut total := i64(0)
    mut i := 0
    for i < data.len - 4 {
        if data[i..i+4] == 'mul(' {
            i += 4
            mut x := 0
            for i < data.len && data[i].is_digit() {
                x = x * 10 + int(data[i] - `0`)
                i++
            }
            if i >= data.len || data[i] != `,` { continue }
            i++
            mut y := 0
            for i < data.len && data[i].is_digit() {
                y = y * 10 + int(data[i] - `0`)
                i++
            }
            if i >= data.len || data[i] != `)` { continue }
            total += x * y
        } else {
            i++
        }
    }
    println(total)
}
