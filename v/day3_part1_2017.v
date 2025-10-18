
import math
import os

fn main() {
    data := os.read_file('input.txt') or { panic('File reading error') }
    target := data.int()
    mut side := int(math.ceil(math.sqrt(f64(target))))
    if side % 2 == 0 {
        side++
    }
    edge := (side - 1) / 2
    max := side * side
    mut best := max
    for i := 0; i < 4; i++ {
        mid := max - edge - (side - 1) * i
        d := if target > mid { target - mid } else { mid - target }
        if d < best {
            best = d
        }
    }
    println(edge + best)
}
