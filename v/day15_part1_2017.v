
import os

fn main() {
    mut buf := os.read_file('input.txt') or { panic(err) }
    lines := buf.trim_space().split_into_lines()
    mut gen_a := lines[0].i64()
    mut gen_b := lines[1].i64()

    gen_a_factor := i64(16807)
    gen_b_factor := i64(48271)
    modulus := i64(2147483647)

    mut matches := 0
    for _ in 0 .. 40000000 {
        gen_a = (gen_a * gen_a_factor) % modulus
        gen_b = (gen_b * gen_b_factor) % modulus
        if u32(gen_a) & 0xFFFF == u32(gen_b) & 0xFFFF {
            matches++
        }
    }
    println(matches)
}
