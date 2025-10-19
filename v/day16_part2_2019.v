
import os

const input_repeat = 10000
const phases = 100

fn main() {
    input := os.read_file('input.txt') or { panic(err) }.trim_space()
    offset := input[..7].int()
    len := input.len
    mut repeated := []int{len: len * input_repeat}
    for i in 0 .. repeated.len {
        repeated[i] = int(input[i % len] - `0`)
    }
    for _ in 0 .. phases {
        mut sum := 0
        for i := repeated.len - 1; i >= offset; i-- {
            sum += repeated[i]
            repeated[i] = sum % 10
        }
    }
    print(repeated[offset..offset + 8].map(fn (v int) string { return v.str() }).join(''))
}
