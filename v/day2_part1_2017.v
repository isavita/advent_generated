
import os

fn main() {
    mut sum := 0
    for line in os.read_lines('input.txt') or { panic(err) } {
        mut min := int(1 << 31 - 1)
        mut max := -min
        for tok in line.split_any(' \t') {
            n := tok.int()
            if n < min { min = n }
            if n > max { max = n }
        }
        sum += max - min
    }
    println(sum)
}
