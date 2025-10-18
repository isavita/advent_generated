
import os

fn main() {
    mut f := os.read_file('input.txt') or { panic('err') }
    lines := f.split_into_lines()
    mut a := lines[0].i64()
    mut b := lines[1].i64()

    mut cnt := 0
    for _ in 0 .. 5_000_000 {
        for {
            a = (a * 16807) % 0x7fffffff
            if a % 4 == 0 { break }
        }
        for {
            b = (b * 48271) % 0x7fffffff
            if b % 8 == 0 { break }
        }
        if u32(a) & 0xffff == u32(b) & 0xffff {
            cnt++
        }
    }
    println(cnt)
}
