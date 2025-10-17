
import os

fn next_sequence(sequence string) string {
    mut res := []u8{cap: sequence.len * 2}
    mut i := 0
    for i < sequence.len {
        ch := sequence[i]
        mut count := 1
        for i + 1 < sequence.len && sequence[i + 1] == ch {
            count++
            i++
        }
        res << u8(`0` + count)
        res << ch
        i++
    }
    return res.bytestr()
}

fn look_and_say(sequence string, iterations int) string {
    mut s := sequence
    for _ in 0 .. iterations {
        s = next_sequence(s)
    }
    return s
}

fn main() {
    buf := os.read_file('input.txt') or { panic(err) }
    initial := buf.trim_space()
    result := look_and_say(initial, 50)
    println(result.len)
}
